;;; pb/km.el -*- lexical-binding: t; -*-

(defun km? (m)
  (and (listp m)
       (or (eq nil m)
           (and (keywordp (car m))
                (km? (cddr m))))))

(defun km (&rest xs)
  (if (km? xs)
      xs
    (error "km build bad args")))

(assert
 (and (and (km? ())
           (km? (list :e 2 :d 4)))
      (not (or (km? (list :e 2 :d 4 90))
         (km? 2)
         (km? (list 4))))))

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

(km-get (km :a (km :b 45))
        (list :a :b))

(km-get (km :a 2)
        :a)

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

(km-put (km :a 1 :b (km :c 3))
        (list :b :c)
        78)


(km-put-in () (list :a :b :c) 3)

(km-put (km :a 1 :b (km :c 3))
        (list :b :d :e)
        78)

(defun km-upd-in (m path f)
  (if path
      (let ((p1 (car path)))
        (if (km? m)
            (plist-put m p1 (km-upd-in (plist-get m p1) (cdr path) f))))
    (f m)))

(defun km-upd (m at f)
  (km-upd-in (copy-tree m)
             (if (listp at) at (if (keywordp at) (list at)))
             f))

(provide 'km)
