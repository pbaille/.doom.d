;;; pb-destructure.el --- Destructuring utils -*- lexical-binding: t; -*-

;; Author: Pierre Baille
;; URL: https://github.com/pbaille
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Destructuring utils.

;;; Code:

(require 'sq)
(require 'km)
(require 'pb)

(defvar pb-destructure_implementations ())

(defun pb-destructure_extend (op implementation)
  "Add a new destructure IMPLEMENTATION for OP.
OP is the first symbol of a pattern.
The IMPLEMENTATION is a lambda that receives the arguments of the pattern
and the seed it has to destructure against."
  (setq pb-destructure_implementations
        (cons (cons op implementation)
              (assq-delete-all op pb-destructure_implementations))))

(defun pb-destructure_guard-symbol? (x)
  (string-suffix-p "?" (pb_name x)))

(defun pb-destructure (pat seed)
  "Produce a list of bindings from PAT and SEED."
  (cond ((and (symbolp pat)
              (not (keywordp pat)))
         (if (equal pat seed)
             ()
           (list (list pat seed))))
        ((consp pat)
         (let* ((op (car pat))
                (args (cdr pat))
                (f (alist-get op pb-destructure_implementations)))
           (cond (f
                  (funcall f args seed))
                 ((pb-destructure_guard-symbol? (car pat))
                  (pb-destructure (cons 'guard pat) seed))
                 (t (error (format "No destructuring implementation for: %s"
                                   op))))))
        (t (list (list (gensym "equal-check_") (list 'equal pat seed))))))

(defun pb-destructure_seed-sym (seed prefix)
  "Generate a symbol for binding the SEED of a destructuration using PREFIX.
If seed is a symbol, gensym is not used and the symbol is returned."
  (if (symbolp seed) seed (gensym prefix)))

(defun pb-destructure_install-builtin-pattern-constructors ()
  "Install the builtin constructors."
  (print "installing destructuring builtins")
  (pb-destructure_extend
   'cons (lambda (args seed)
           (let ((sym (pb-destructure_seed-sym seed "cons_")))
             (append (pb-destructure sym seed)
                     (pb-destructure (car args) (list 'car sym))
                     (pb-destructure (cadr args) (list 'cdr sym))))))

  (pb-destructure_extend
   'list (lambda (args seed)
           (let ((sym (pb-destructure_seed-sym seed "list_")))
             (append (pb-destructure sym seed)
                     (sq_join (cl-loop for i from 0 to (- (length args) 1)
                                       collect (pb-destructure (nth i args) (list 'nth i sym))))))))

  (pb-destructure_extend
   'list* (lambda (args seed)
            (let ((sym (pb-destructure_seed-sym seed "list_"))
                  (head (gensym "head_")))
              (append (pb-destructure sym seed)
                      (list (list head (list 'sq_take-strict sym (- (length args) 1))))
                      (pb-destructure (cons 'list (sq_butlast args)) head)
                      (list (list (sq_last args) (list 'seq-drop sym (- (length args) 1))))))))

  (pb-destructure_extend
   'and (lambda (args seed)
          (let ((sym (pb-destructure_seed-sym seed "and-seed_")))
            (append (pb-destructure sym seed)
                    (sq_join (mapcar (lambda (pat) (pb-destructure pat sym))
                                     args))))))

  (pb-destructure_extend
   'as (lambda (args seed)
         (pb-destructure (cons 'and args) seed)))

  (pb-destructure_extend
   'eq (lambda (args seed)
         (pb-destructure (car args)
                         `(if (equal ,(cadr args) ,seed) ,seed))))

  (pb-destructure_extend
   'guard (lambda (args seed)
            (pb-destructure (cadr args)
                            `(if (,(car args) ,seed) ,seed))))

  (pb-destructure_extend
   'km (lambda (args seed)
         (let ((sym (pb-destructure_seed-sym seed "plist_")))
           (append (pb-destructure sym seed)
                   (sq_join (mapcar (lambda (entry)
                                      (pb-destructure (cdr entry)
                                                      (list 'plist-get sym (car entry))))
                                    (km_parse-free-form args)))))))

  (pb-destructure_extend
   'km_keys (lambda (args seed)
              (pb-destructure (cons 'km (sq_interleave (mapcar #'pb_symbol-to-keyword args)
                                                       args))
                              seed)))

  (pb-destructure_extend
   'km_at (lambda (args seed)
            (pb-destructure (cadr args)
                            (list 'km_get seed (car args))))))

(pb-destructure_install-builtin-pattern-constructors)

(defmacro pb-destructure_let (bindings &rest body)
  "Destructuring let, unlike let and let*, BINDINGS are a flat list.
BODY expressions are evaluated in those new bindings, the last value
is returned."
  (declare (indent 1))
  `(let* ,(sq_join (mapcar (lambda (binding)
                             (pb-destructure (car binding) (cadr binding)))
                           (sq_partition 2 2 (append bindings nil))))
     ,@body))

(defmacro pb-destructure_fn (&rest decl)
  "Create a lambda function that use the `pb-destructure_let' for its arguments.
DECL can be prefixed by a name for the lambda and a docstring.
Followed by the args pattern and expressions."
  (pb-destructure_let [(list* name argv xs) (if (symbolp (car decl)) decl (cons nil decl))
                       (cons doc body) (if (stringp (car xs)) xs (cons nil xs))
                       arglist (append argv ())
                       argsyms (mapcar (lambda (x) (if (symbolp x) x (gensym "arg_")))
                                       arglist)
                       compiled-body `(,@(if doc (list doc))
                                       (pb-destructure_let ,(sq_interleave arglist argsyms)
                                           ,@body))]
      (if name
          `(cl-labels ((,name ,argsyms ,@compiled-body))
             (function ,name))
        `(lambda ,argsyms ,@compiled-body))))

(defmacro pb-destructure_defun (name &rest fn-decl)
  "Like `defun' but use `pb-destructure_fn' under the hood.
NAME is the top level name the lambda will be bound to.
FN-DECL is the same kind of arguments `pb-destructure_fn' expects."
  (declare (indent defun))
  `(defalias ',name (pb-destructure_fn ,@fn-decl)))

(defun pb-destructure_test ()
  "Test some of the functionalities."

  (cl-assert
   (and (pb-destructure_guard-symbol? 'poi?)
        (null (pb-destructure_guard-symbol? 'poi))))

  (cl-assert
   (equal (pb-destructure '(pop? x) 'io)
          '((x (if (pop? io) io)))))

  (cl-assert
   (and (equal (pb-destructure_let [(cons a b) (list 1 2 3 4)
                                    (list (cons x y) z) (list (cons a :io) :bop b)
                                    (and xs (list* j (cons k1 k2) l)) (list 1 (cons 3 4) 2)]
                 (list a b x y z j k1 k2 l xs))
               '(1 (2 3 4) 1 :io :bop 1 3 4 (2) (1 (3 . 4) 2)))

        (equal (funcall (pb-destructure_fn (a b c) (list a b c))
                        1 2 3)
               (list 1 2 3))

        (equal (funcall (pb-destructure_fn ((list* a b xs)) (list a b xs))
                        (list 1 2 3 4))
               (list 1 2 (list 3 4)))

        (equal (pb-destructure_let [(list* a b c xs) (list 2 3)]
                   (list a b c xs))
               (list nil nil nil nil))

        (equal (pb-destructure_let [(list* a b xs) (list 2 3)]
                   (list a b xs))
               '(2 3 nil))

        (equal (pb-destructure_let [(eq a 2) (+ 1 1)]
                   a)
               2)

        (equal (pb-destructure_let [x 2
                                      (eq a (list 1 x 3)) (list 1 2 3)]
                                   (list a x))
               (list (list 1 2 3) 2))

        (equal (pb-destructure_let [x 3
                                      (eq a (list 1 x 3)) (list 1 2 3)]
                 (list a x))
               (list nil 3))

        (equal (pb-destructure_let [(km :a (list* a b xs) :c c) (km :a (list 1 2 3) :c 34)]
                   (list a b c xs))
               '(1 2 34 (3)))

        (equal (pb-destructure_let [(km c :a (list* a b xs)) (km :a (list 1 2 3) :c 34)]
                   (list a b c xs))
               '(1 2 34 (3)))

        (equal (pb-destructure_let [(km a b c :extra d) (km :a 1 :b 2 :c 3 :extra 4)]
                   (list a b c d))
               (list 1 2 3 4))

        (equal (pb-destructure_let [(km_keys a b c) (km :a 1 :b 2 :c 3)]
                   (list a b c))
               (list 1 2 3))

        (equal (list 1 2 (list 3 4))
               (pb-destructure_let [(list* a b xs) (list 1 2 3 4)]
                   (list a b xs)))

        (equal (funcall (pb-destructure_fn [(list* a b xs)]
                                           (list a b xs))
                        (list 1 2 3 4))
               (list 1 2 (list 3 4)))
        (equal (pb-destructure_let [(as m (km_keys a)) (km :a 1 :b 2)]
                   (list m a))
               '((:a 1 :b 2) 1)))))

(pb-destructure_test)

(defalias 'pb_let 'pb-destructure_let)
(defalias 'pb_fn 'pb-destructure_fn)
(defalias 'pb_defun 'pb-destructure_defun)

(provide 'pb-destructure)
;;; pb-destructure.el ends here.
