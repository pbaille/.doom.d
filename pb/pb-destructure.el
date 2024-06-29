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

(defvar pb-destructure_implementations ())

(defun pb-destructure_extend (op implementation)
  "Add a new destructure IMPLEMENTATION for OP.
OP is the first symbol of a pattern.
The IMPLEMENTATION is a lambda that receives the arguments of the pattern
and the seed it has to destructure against."
  (setq pb-destructure_implementations
        (cons (cons op implementation)
              (assq-delete-all op pb-destructure_implementations))))

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
           (if f
               (funcall f args seed)
             (error (format "No destructuring implementation for: %s"
                            op)))))
        (t (list (list (gensym "equal-check_") (list 'equal pat seed))))))

(defmacro pb-destructure_let (bindings &rest body)
  "Destructuring let, unlike let and let*, BINDINGS are a flat list.
BODY expressions are evaluated in those new bindings, the last value
is returned."
  `(let* ,(sq_join (mapcar (lambda (binding)
                             (pb-destructure (car binding) (cadr binding)))
                           (sq_partition 2 2 (append bindings nil))))
     ,@body))

(defmacro pb-destructure_fn (pat &rest body)
  "Destructuring lambda, use `pb-destructure_let' to bind its arguments using PAT.
Then execute its BODY within those bindings."
  (let* ((docstring (if (and (cdr body) (stringp (car body)))
                        (car body)) )
         (body (if docstring (cdr body) body))
         (pat (append pat ()))
         (argsyms (mapcar (lambda (x) (if (symbolp x) x (gensym "arg_")))
                          pat)))
    `(lambda ,argsyms
       ,@(if docstring (list docstring) ())
       (pb-destructure_let ,(sq_interleave pat argsyms)
                           ,@body))))

(defmacro pb-destructure_defun (name &rest fn-decl)
  "Like `defun' but use `pb-destructure_fn' under the hood.
NAME is the top level name the lambda will be bound to.
FN-DECL is the same kind of arguments `pb-destructure_fn' expects."
  (declare (indent defun))
  `(defalias ',name (pb-destructure_fn ,@fn-decl)))

(defun pb-destructure_seed-sym (seed prefix)
  "Generate a symbol for binding the SEED of a destructuration using PREFIX.
If seed is a symbol, gensym is not used and the symbol is returned."
  (if (symbolp seed) seed (gensym prefix)))

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
 'km (lambda (args seed)
        (let ((sym (pb-destructure_seed-sym seed "plist_")))
          (append (pb-destructure sym seed)
                  (sq_join (mapcar (lambda (entry)
                                     (pb-destructure (cdr entry)
                                                     (list 'plist-get sym (car entry))))
                                   (km_entries args)))))))

(pb-destructure_extend
 'km_keys (lambda (args seed)
            (pb-destructure (cons 'km (sq_interleave (mapcar #'pb_symbol-to-keyword args)
                                                     args))
                            seed)))

(pb-destructure_extend
 'km_at (lambda (args seed)
          (pb-destructure (cadr args)
                          (list 'km_get seed (car args)))))

(defun pb-destructure_test ()
  "Test some of the functionalities."

  (equal (pb-destructure_let [(cons a b) (list 1 2 3 4)
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

  (equal (pb-destructure_let [(km :a (list* a b xs) :c c) (km :a (list 1 2 3) :c 34)]
                             (list a b c xs))
         '(1 2 34 (3)))

  (equal (pb-destructure_let [(km_keys a b c) (km :a 1 :b 2 :c 3)]
                             (list a b c))
         (list 1 2 3)))

(pb-destructure_test)

(provide 'pb-destructure)
;;; pb-destructure.el ends here.
