;;; pb-flow.el --- flow utils -*- lexical-binding: t; -*-

;; Author: Pierre Baille
;; URL: https://github.com/pbaille
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Flow utils.

;;; Code:

(require 'pb)
(require 'pb-destructure)

(defun pb-flow/thunk-symbols (n)
  "Generating N symbols to hold case thunks."
  (mapcar (lambda (x)
            (gensym (format "case_%s_" x)))
          (sq_range 0 n)))

(pb/defun pb-flow/compile-case
  [(and case
        (km_keys test bindings return next))]
  "Compile a single case from a case map.
A compiled case is an if-expression, choosing between return value
or falling through to the next case."
  (let ((cont (when next (list next))))
    (cond
     (test `(if ,test ,return ,cont))
     (bindings (pb/let [(cons (list b1 b2) bs) bindings]
                       `(if-let ((,b1 ,b2))
                            ,(pb-flow/compile-case (km_put case :bindings bs))
                          ,cont)))
     (t return))))

(defun pb-flow/cases->thunks (cases)
  "Convert a list of CASES into thunk expressions.
Converts each case into a list of three elements:
symbol, empty list, and a compiled case."
  (mapcar (lambda (case)
            (list (km_get case :symbol) () (pb-flow/compile-case case)))
        cases))

(defun pb-flow/normalize-body (body)
  "Normalize the given BODY ensuring it has an even number of elements.
If the count of elements is odd, add a bottom case with a nil return."
  (if (cl-oddp (length body))
      (append (sq_butlast body) (list :pb-flow/bottom (sq_last body)))
    (append body (list :pb-flow/bottom nil))))

(defun pb-flow/body->cases (body)
  "Convert an expression BODY into a list of cases.
Each case is a map containing keys :return, :symbol, :next, :test, :bindings."
  (let* ((normalized-body (pb-flow/normalize-body body))
         (case-count (round (/ (length normalized-body) 2))))
    (seq-mapn (pb/fn [(list left right) (list sym nxt)]
                     (let ((bindings? (vectorp left))
                           (bottom? (equal :pb-flow/bottom left)))
                       (km
                        :return right
                        :symbol sym
                        :next (unless bottom? nxt)
                        :test (unless (or bindings? bottom?) left)
                        :bindings (when bindings? (sq_join (mapcar (pb/fn [(list pat seed)]
                                                                          (pb-destructure pat seed))
                                                                   (sq_partition 2 2 (append left ()))))) )))
              (sq_partition 2 2 normalized-body)
              (sq_partition 2 1 (append (pb-flow/thunk-symbols case-count)
                                        (list nil))))))

(defun pb-flow/emit-form (body)
  "Transform a normalized BODY into a form.
The resulting form is either a single return value or a `cl-labels' expression."
  (let* ((thunks (pb-> (pb-flow/body->cases body) pb-flow/cases->thunks))
         (return (nth 2 (car thunks))))
    (if-let ((bindings (cdr thunks)))
        `(cl-labels ,bindings ,return)
      return)))

(defmacro pb-flow (&rest body)
  "Emit a flow control form from the provided BODY code blocks."
  (declare (indent nil))
  (pb-flow/emit-form body))

(defmacro pb-flow/fn (&rest decl)
  "Create a lambda function that use the `pb-flow' control.
DECL can be prefixed by a name for the lambda and a docstring.
Followed by a flat serie of cases of the form args-pattern return-expr."
  (pb/let [(cons name xs) (if (symbolp (car decl)) decl (cons nil decl))
           (cons doc body) (if (stringp (car xs)) xs (cons nil xs))
           argsym (gensym "args_")
           compiled-body `(,@(if doc (list doc))
                           (pb-flow ,@(sq_join (mapcar (pb/fn [(list pat ret)]
                                                              (list (vector (cons 'list (append pat ())) argsym) ret))
                                                       (sq_partition 2 2 body)))))]
          (if name
              `(cl-labels ((,name (&rest ,argsym)
                             ,@compiled-body))
                 (function ,name))
            `(lambda (&rest ,argsym)
               ,@compiled-body))))

(defun pb-flow/tests ()
  "Some assertions."
  (cl-assert
   (and (not (pb-flow (equal 3 1) :ok))
        (equal :ok (pb-flow (equal 1 1) :ok))
        (equal 6
               (pb-flow [a (km_get (km :a 3) :a)]
                        (+ a a)
                        :fail))
        (equal :fail
               (pb-flow [a (km_get (km :a 3) :a)
                           b (if (> 0 a) (- a))]
                        (+ a a)
                        :fail))

        (null (pb-flow [(cons 'io x) (cons 'iop 2)]
                x))

        (equal (pb-flow [x (cons 'io 23)
                           (cons 'io x) x]
                        x)
               23)

        (pb/let [f (pb/fn [x]
                          (pb-flow (> x 0) (list :pos x)
                                   (< x 0) (list :neg x)
                                   :zero))]
          (and (equal :zero (funcall f 0))
               (equal (list :pos 1) (funcall f 1))
               (equal (list :neg -1) (funcall f -1))))
        (equal (let ((f (pb-flow/fn
                         [(list :pair x y)] (list :pair (km :left x :right y))
                         [(list :atom x)] (list :atom x)
                         [(cons x :pouet)] (list :case3 x))))
                 (list (funcall f (list :pair 1 2))
                       (funcall f (list :atom 2))
                       (funcall f (cons :yop :pouet))))
               '((:pair (:left 1 :right 2)) (:atom 2) (:case3 :yop)))
        (equal (list 5 4 3 2 1)
               (funcall (pb-flow/fn rec [x] (if (> x 0)
                                                (cons x (rec (- x 1)))))
                        5)))))

(pb-flow/tests)

(defalias 'pb/if 'pb-flow)
(defalias 'pb/fm 'pb-flow/fn)

(provide 'pb-flow)
;;; pb-flow.el ends here.
