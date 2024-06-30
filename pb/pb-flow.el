;;; pb-flow.el --- flow utils -*- lexical-binding: t; -*-

;; Author: Pierre Baille
;; URL: https://github.com/pbaille
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Flow utils.

;;; Code:

(require 'pb)

(defun pb-flow_thunk-symbols (n)
  "generating symbols to hold case thunks"
  (mapcar (lambda (x)
            (gensym (format "case_%s_" x)))
          (sq_range 0 n)))

(pb_defun pb-flow_compile-case
  [(and case
        (km_keys test bindings return next))]
  "Compile a single case from a case map.
A compiled case is an if-expression, choosing between return value
or falling through to the next case."
  (let ((cont (when next (list next))))
    (cond
     (test `(if ,test ,return ,cont))
     (bindings (pb_let [(cons (list b1 b2) bs) bindings]
                       `(if-let ((,b1 ,b2))
                            ,(pb-flow_compile-case (km_put case :bindings bs))
                          ,cont)))
     (t return))))

(defun pb-flow_cases->thunks (cases)
  "Convert a list of cases into thunk expressions.
Converts each case into a list of three elements:
symbol, empty list, and a compiled case."
  (mapcar (lambda (case)
            (list (km_get case :symbol) () (pb-flow_compile-case case)))
        cases))

(defun pb-flow_normalize-body (body)
  "Normalize the given body ensuring it has an even number of elements.
If the count of elements is odd, add a bottom case with a nil return."
  (if (cl-oddp (length body))
      (append (sq_butlast body) (list :pb-flow_bottom (sq_last body)))
    (append body (list :pb-flow_bottom nil))))

(defun pb-flow_body->cases (body)
  "Convert an expression body into a list of cases.
Each case is a map containing keys :return, :symbol, :next, :test, :bindings."
  (let* ((normalized-body (pb-flow_normalize-body body))
         (case-count (round (/ (length normalized-body) 2))))
    (seq-mapn (pb_fn [(list left right) (list sym nxt)]
                     (let ((bindings? (vectorp left))
                           (bottom? (equal :pb-flow_bottom left)))
                       (km
                        :return right
                        :symbol sym
                        :next (unless bottom? nxt)
                        :test (unless (or bindings? bottom?) left)
                        :bindings (when bindings? (sq_join (mapcar (pb_fn [(list pat seed)]
                                                                          (pb-destructure pat seed))
                                                                   (sq_partition 2 2 (append left ()))))) )))
              (sq_partition 2 2 normalized-body)
              (sq_partition 2 1 (pb-flow_thunk-symbols case-count)))))

(defun pb-flow_emit-form (body)
  "Transform a normalized body into a form.
The resulting form is either a single return value or a cl-labels expression."
  (let* ((thunks (pb-> (pb-flow_body->cases body) pb-flow_cases->thunks))
         (return (nth 2 (first thunks))))
    (if-let ((bindings (cdr thunks)))
        `(cl-labels ,bindings ,return)
      return)))

(defmacro pb-flow (&rest body)
  "Emit a flow control form from the provided BODY code blocks."
  (pb-flow_emit-form body))

(defmacro pb-flow_fn (&rest decl)
  "Create a lambda function that uses the pb-flow flow control.
DECL can be prefixed by a name for the lambda and a docstring.
Followed by a flat serie of cases of the form args-pattern return-expr."
  (pb_let [(cons name xs) (if (symbolp (car decl)) decl (cons nil decl))
           (cons doc body) (if (stringp (car xs)) xs (cons nil xs))
           argsym (gensym "args_")]
          `(lambda (&rest ,argsym)
             (pb-flow ,@(sq_join (mapcar (pb_fn [(list pat ret)]
                                                (list (vector (cons 'list (append pat ())) argsym) ret))
                                         (sq_partition 2 2 body)))))))

(defun pb-flow_tests ()
  "Some little checks."
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
       (pb_let [f (pb_fn [x]
                         (pb-flow (> x 0) (list :pos x)
                                  (< x 0) (list :neg x)
                                  :zero))]
               (and (equal :zero (funcall f 0))
                    (equal (list :pos 1) (funcall f 1))
                    (equal (list :neg -1) (funcall f -1))))
       (equal (let ((f (pb-flow_fn
                        [(list :pair x y)] (list :pair (km :left x :right y))
                        [(list :atom x)] (list :atom x)
                        [(cons x :pouet)] (list :case3 x))))
                (list (funcall f (list :pair 1 2))
                      (funcall f (list :atom 2))
                      (funcall f (cons :yop :pouet))))
              '((:pair (:left 1 :right 2)) (:atom 2) (:case3 :yop)))))

(pb-flow_tests)

(provide 'pb-flow)
;;; pb-flow.el ends here.
