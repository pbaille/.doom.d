;;; pb/pb-walk.el -*- lexical-binding: t; -*-

(defun pb-walk (x inner outer)
  (if (proper-list-p x)
      (funcall outer
       (mapcar inner x))
    (funcall outer x)))

(defun pb-postwalk (x f)
  (pb-walk x (lambda (y) (pb-postwalk y f)) f))

(defun pb-prewalk (x f)
  (pb-walk (funcall f x)
           (lambda (y)
             (pb-prewalk y f))
           #'identity))

(defun pb-map-indexed (xs f)
  (cl-loop for val in xs
           for idx from 0
           collect (funcall f idx val)))

'(:comment
  "those do not terminate and I don't understand why"
  (defun pb-indexed-walk (x at inner outer)
    (if (listp x)
        (funcall outer
                 at
                 (pb-map-indexed x
                                 (lambda (i x) (funcall inner (append at (list i)) x))))
      (funcall outer at x)))

  (defun pb-indexed-postwalk (x f)
    (letrec ((postwalk (lambda (x at f)
                         (pb-indexed-walk x at
                                          (lambda (at y)
                                            (funcall postwalk y at f))
                                          f))))
      (funcall postwalk x () f)))

  (defun pb-indexed-prewalk (x f)
    (letrec ((prewalk (lambda (x at f)
                        (pb-indexed-walk (funcall f at x)
                                         at
                                         (lambda (at y)
                                           (funcall prewalk y at f))
                                         (lambda (_ y) y)))))
      (funcall prewalk x () f))))

'(:comment
  (pb-postwalk `(a (b c (d e)) b)
               (lambda (x) (print x) x))
  (pb-indexed-prewalk `(a (b c (d e)) b)
                      (lambda (i x) (print (list i x)) x))
  (pb-map-indexed (list 1 2 3) #'cons)
  (pb-prewalk `(a (b c (d e)) b)
              (lambda (x) (print x) x)))

(provide 'pb-walk)
