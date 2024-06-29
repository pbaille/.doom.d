;;; pb/pb-flow.el -*- lexical-binding: t; -*-

(defun pb-flow_thunk-symbols (n)
  "generating symbols to hold case thunks"
  (mapcar (lambda (x)
            (gensym (format "case_%s_" x)))
          (sq_range 0 n)))

(pb_defun pb-flow_compile-case
  [(and case
        (km_keys test bindings return next))]
  (let ((cont (when next (list next))))
    (cond
     (test `(if ,test ,return ,cont))
     (bindings (pb_let [(cons (list b1 b2) bs) bindings]
                       `(if-let ((,b1 ,b2))
                            ,(pb-flow_compile-case (km_put case :bindings bs))
                          ,cont)))
     (t return))))

(defun pb-flow_cases->thunks (cases)
  (mapcar (lambda (case)
            (list (km_get case :symbol) () (pb-flow_compile-case case)))
        cases))

(defun pb-flow_normalize-body (body)
  (if (cl-oddp (length body))
      (append (sq_butlast body) (list :pb-flow_bottom (sq_last body)))
    (append body (list :pb-flow_bottom nil))))

(defun pb-flow_body->cases (body)
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
  ""
  (let* ((thunks (pb-> (pb-flow_body->cases body) pb-flow_cases->thunks))
         (return (nth 2 (first thunks))))
    (if-let ((bindings (cdr thunks)))
        `(cl-labels ,bindings ,return)
      return)))

(defmacro pb-flow (&rest body)
  ""
  (pb-flow_emit-form body))

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
                    (equal (list :neg -1) (funcall f -1))))))

(pb-flow_tests)
