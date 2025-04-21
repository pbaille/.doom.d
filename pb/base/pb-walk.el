;;; pb-walk.el --- list walk helpers -*- lexical-binding: t; -*-

;; Author: Pierre Baille
;; URL: https://github.com/pbaille
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; list walk helpers.

;;; Code:

(defun pb-walk (x inner outer)
  (if (proper-list-p x)
      (funcall outer
       (mapcar inner x))
    (funcall outer x)))

(defun pb-walk/post (x f)
  (pb-walk x (lambda (y) (pb-walk/post y f)) f))

(defun pb-walk/pre (x f)
  (pb-walk (funcall f x)
           (lambda (y)
             (pb-walk/pre y f))
           #'identity))

'(:comment
  (defun pb-map-indexed (xs f)
    (cl-loop for val in xs
             for idx from 0
             collect (funcall f idx val)))
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
  (pb-walk/post `(a (b c (d e)) b)
               (lambda (x) (print x) x))
  (pb-indexed-prewalk `(a (b c (d e)) b)
                      (lambda (i x) (print (list i x)) x))
  (pb-map-indexed (list 1 2 3) #'cons)
  (pb-walk/pre `(a (b c (d e)) b)
              (lambda (x) (print x) x)))

(provide 'pb-walk)
;;; pb-walk.el ends here.
