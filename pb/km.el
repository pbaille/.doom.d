;;; km.el --- Keyword maps (plist) utils -*- lexical-binding: t; -*-

;; Author: Pierre Baille
;; URL: https://github.com/pbaille
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Utils.

;;; Code:

(require 'cl-lib)
(require 'sq)

(defun km? (m)
  "Check if M is a keyword map."
  (and (listp m)
       (or (eq nil m)
           (and (keywordp (car m))
                (km? (cddr m))))))

(defun km (&rest xs)
  "Build a keyword map from XS.
Throws an error if XS does not form a valid keyword map."
  (if (km? xs)
      xs
    (error (format "Bad argument to km: %s" xs))))

(defun km_entries (m)
  "Return the entries of the keyword map M as pairs (key . value)."
  (if (consp m)
      (cons (cons (car m) (cadr m))
            (km_entries (cddr m)))))

(defun km_into (m entries)
  "Add some ENTRIES to M (plist/keyword-map)."
  (cl-reduce (lambda (m entry)
               (plist-put m (car entry) (cdr entry)))
             entries
             :initial-value m))

(defun km_merge (&rest kms)
  "Merge several KMS together."
  (cl-reduce (lambda (ret x) (km_into ret (km_entries x)))
             kms
             :initial-value ()))

(defun km_map (m f)
  "Map F over M. F takes and return an entry (a cons of keyword and value)."
  (km_into
   ()
   (mapcar f (km_entries m))))

(defun km_map-keys (m f)
  "Map F over keys of M. F takes and return a keyword."
  (km_map m (lambda (e) (cons (funcall f (car e)) (cdr e)))))

(defun km_map-vals (m f)
  "Map F over values of M."
  (km_map m (lambda (e) (cons (car e) (funcall f (cdr e))))))

(defun km_keys (m)
  "Return the keys of the keyword map M."
  (and (km? m)
       (mapcar #'car (km_entries m))))

(defun km_vals (m)
  "Return the values of the keyword map M."
  (and (km? m)
       (mapcar #'cdr (km_entries m))))

(defun km_get-in (m path)
  "Get the value at PATH in the nested keyword map M."
  (if (and m path)
      (km_get-in (plist-get m (car path)) (cdr path))
    m))

(defun km_get (m at)
  "Get the value at AT in the keyword map M.
If AT is a list, get the value in a nested map."
  (cond ((listp at) (km_get-in m at))
        ((keywordp at) (plist-get m at))))

(defun km_put-in (m path v)
  "Associate PATH in the nested keyword map M with value V."
  (if path
      (let ((p1 (car path)))
        (if (km? m)
            (plist-put m p1 (km_put-in (plist-get m p1) (cdr path) v))))
    v))

(defun km_put1 (m at v)
  "Put value V at AT in a copy of keyword map M.
If AT is a list, put the value in a nested map."
  (km_put-in m
             (cond ((listp at) at)
                   ((keywordp at) (list at)))
             v))

(defun km_put (m &rest xs)
  "Associates keys with values in keyword map M using XS."
  (cl-reduce (lambda (m entry)
               (km_put1 m (car entry) (cadr entry)))
             (sq_partition 2 2 xs)
             :initial-value m))

(defun km_upd-in (m path f)
  "Update the value at PATH in the nested keyword map M by applying function F."
  (if path
      (let ((p1 (car path)))
        (if (km? m)
            (plist-put m p1 (km_upd-in (plist-get m p1) (cdr path) f))))
    (funcall f m)))

(defun km_upd1 (m at f)
  "Update value at AT in a copy of keyword map M by applying function F.
If AT is a list, update the value in a nested map."
  (km_upd-in m
             (cond ((listp at) at)
                   ((keywordp at) (list at)))
             f))

(defun km_upd (m &rest xs)
  "Update the keyword map M using XS.
XS is a list alternating paths and update-fns."
  (cl-reduce (lambda (m entry)
               (km_upd1 m (car entry) (cadr entry)))
             (sq_partition 2 2 xs)
             :initial-value m))

(defun km_all-paths (m)
  "Transform M into an alist of path -> value."
  (cl-reduce (lambda (ret entry)
                 (let ((k (car entry))
                       (v (cdr entry)))
                   (append ret
                           (if (km? v)
                               (mapcar (lambda (e)
                                         (cons (cons k (car e))
                                               (cdr e)))
                                       (km_all-paths v))
                             (list (cons (list k) v))))))
             (km_entries m)
             :initial-value ()))

(defmacro km_let (binding &rest body)
  "Let binding for keyword maps.
BINDING specifies variables to be bound from a keyword map,
and BODY is the code to execute in the context of those bindings."
  (let* ((pat (car binding))
         (pat (if (equal :as (car pat))
                  (list (cadr pat) (cddr pat))
                (list (gensym) pat)))
         (seedsym (car pat))
         (ks (cadr pat))
         (seed (cadr binding)))
    `(let* ((,seedsym ,seed)
            ,@(mapcar (lambda (k) (list k `(plist-get ,seedsym ,(intern (concat ":" (symbol-name k))))))
                      ks))
       ,@body)))

(defmacro km_lambda (ks &rest body)
  "Define a function that takes a keyword map as its sole argument.
KS specifies the expected keys, and BODY is the code to execute."
  (let ((seed (gensym)))
    `(lambda (,seed)
       (km_let (,ks ,seed)
                ,@body))))

(defmacro km_defun (name ks &rest body)
  "Define a named function that takes a keyword map as its sole argument.
NAME is the function name, KS specifies the expected keys,
and BODY is the code to execute."
  (let ((seed (gensym)))
    `(defun ,name (,seed)
       (km_let (,ks ,seed)
               ,@body))))

(defmacro kmq (&rest keys)
  "Build a km using binding names as KEYS."
  (cons 'list (km_into ()
                       (mapcar (lambda (k) (cons (intern (concat ":" (symbol-name k)))
                                            k))
                               keys))))

(defun km_test ()
  "Run some assertions for the keyword map functions."
  (cl-assert
   (and (and (km? ())
             (km? (list :e 2 :d 4)))
        (not (or (km? (list :e 2 :d 4 90))
                 (km? 2)
                 (km? (list 4))))))

  (cl-assert
   (and (eq (km_get (km :a (km :b 45))
                    (list :a :b))
            45)

        (eq (km_get (km :a 2)
                    :a)
            2)))

  (cl-assert
   (and (equal (km_put '(:a 1 :b (:c 3))
                       (list :b :c)
                       78)
               '(:a 1 :b (:c 78)))

        (equal (km_put-in () (list :a :b :c) 3)
               '(:a (:b (:c 3))))

        (equal (km_put (km :a 1 :b (km :c 3))
                       (list :b :d :e)
                       78)
               '(:a 1 :b (:c 3 :d (:e 78))))))

  (cl-assert
   (and (equal (km_upd '(:a 1) :a (lambda (x) (+ x 1)))
               '(:a 2))
        (equal (km_upd '(:a 1 :b (:c 0)) '(:b :c) (lambda (x) (+ x 1)))
               '(:a 1 :b (:c 1)))
        (equal (km_upd '(:a 1) '(:b :c) (lambda (x) (or x 32)))
               '(:a 1 :b (:c 32)))))

  (cl-assert
   (equal (km_let ((a b) (km :a 1 :b 2))
                  (+ a b))
          3)
   (equal (km_let ((:as m a b) (km :a 1 :b 2))
                  (km_put m :ret (+ a b)))
          (km :a 1 :b 2 :ret 3))
   (equal (funcall (km_lambda (a b) (+ a b))
                   (km :a 1 :b 3))
          4))

  (cl-assert
   (equal (km_into (km :a 2)
                   (km_entries (km :b 2 :c 4)))
          (km :a 2 :b 2 :c 4)))

  (cl-assert
   (equal (km_all-paths (km :a 2
                            :b (km :c 3 :d 6)
                            :d 6
                            :e (km :f (km :g 4))))
          '(((:a) . 2)
            ((:b :c) . 3)
            ((:b :d) . 6)
            ((:d) . 6) ((:e :f :g) . 4))))

  (cl-assert
   (let ((a 1)(b 2))
     (equal (kmq a b)
            (km :a a :b b)))))

(km_test)

(provide 'km)
;;; km.el ends here.
