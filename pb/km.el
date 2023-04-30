;;; pb/km.el -*- lexical-binding: t; -*-

(require 'cl-lib)

(require 'sequences)
(require 'names)

(defun km? (m)
  (and (listp m)
       (or (eq nil m)
           (and (keywordp (car m))
                (km? (cddr m))))))

(defun km (&rest xs)
  (if (km? xs)
      xs
    (error "km build bad args")))

(defun km-entries (m)
  (and (km? m)
       (sq-partition 2 2 m)))

(defun km-keys (m)
  (and (km? m)
       (cl-map (lambda (entry) (car entry)) (entries m))))

(defun km-vals (m)
  (and (km? m)
       (cl-map (lambda (entry) (cadr entry)) (entries m))))

(defun km-get-in (m path)
  (if path
      (let ((found (if (km? m) (plist-get m (car path)))))
        (if found
            (km-get-in found (cdr path))))
    m))

(defun km-get (m at)
  (if (listp at)
      (km-get-in m at)
    (if (keywordp at)
        (plist-get m at))))

(defun km-put-in (m path v)
  (if path
      (let ((p1 (car path)))
        (if (km? m)
            (plist-put m p1 (km-put-in (plist-get m p1) (cdr path) v))))
    v))

(defun km-put (m at v)
  (km-put-in (copy-tree m)
             (if (listp at) at (if (keywordp at) (list at)))
             v))

(defun km-upd-in (m path f)
  (if path
      (let ((p1 (car path)))
        (if (km? m)
            (plist-put m p1 (km-upd-in (plist-get m p1) (cdr path) f))))
    (funcall f m)))

(defun km-upd (m at f)
  (km-upd-in (copy-tree m)
             (if (listp at) at (if (keywordp at) (list at)))
             f))

(defmacro km-letks (binding &rest body)
  (let ((ks (car binding))
        (seed (cadr binding)))
    `(let* ((let-keys_seed ,seed)
            ,@(cl-mapcar (lambda (k) (list k `(plist-get let-keys_seed ,(symbol-to-keyword k))))
                         ks))
       ,@body)))

(defun km-test ()
  (cl-assert
   (and (and (km? ())
             (km? (list :e 2 :d 4)))
        (not (or (km? (list :e 2 :d 4 90))
                 (km? 2)
                 (km? (list 4))))))

  (cl-assert
   (and (eq (km-get (km :a (km :b 45))
                    (list :a :b))
            45)

        (eq (km-get (km :a 2)
                    :a)
            2)))

  (cl-assert
   (and (equal (km-put '(:a 1 :b (:c 3))
                       (list :b :c)
                       78)
               '(:a 1 :b (:c 78)))

        (equal (km-put-in () (list :a :b :c) 3)
               '(:a (:b (:c 3))))

        (equal (km-put (km :a 1 :b (km :c 3))
                       (list :b :d :e)
                       78)
               '(:a 1 :b (:c 3 :d (:e 78))))))

  (cl-assert
   (and (equal (km-upd '(:a 1) :a (lambda (x) (+ x 1)))
               '(:a 2))
        (equal (km-upd '(:a 1 :b (:c 0)) '(:b :c) (lambda (x) (+ x 1)))
               '(:a 1 :b (:c 1)))
        (equal (km-upd '(:a 1) '(:b :c) (lambda (x) (or x 32)))
               '(:a 1 :b (:c 32)))))
  (cl-assert
   (equal (km-letks ((a b) (km :a 1 :b 2))
                    (+ a b))
          3)))

(km-test)

(provide 'km)
