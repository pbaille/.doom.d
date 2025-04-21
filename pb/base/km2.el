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
(require 'pb-struct)

(defun km2-content? (m)
  "Check if M is a keyword map."
  (and (listp m)
       (or (null m)
           (and (keywordp (car m))
                (km2-content? (cddr m))))))

(pb-struct_deftag km2 #'km2-content?)

(defun km2/content->entries (pl)
  ""
  (when pl
    (cons (cons (car pl) (cadr pl))
          (km2/content->entries (cddr pl)))))

(defun km2/entries (m)
  "Return the entries of the keyword map M as pairs (key . value)."
  (if (km2? m)
      (km2/content->entries (cdr m))))

(defun km2/keys (m)
  "Return the keys of the keyword map M."
  (and (km2? m)
       (mapcar #'car (km2/entries m))))

(defun km2/vals (m)
  "Return the values of the keyword map M."
  (and (km2? m)
       (mapcar #'cdr (km2/entries m))))

(defun km2/eq (a b)
  "Check if two keyword maps A and B are equal.
Keyword maps are considered equal if they contain the same keys
with the same associated values."
  (and (km2? a)
       (km2? b)
       (let ((keys1 (km2/keys a))
             (keys2 (km2/keys b)))
         (and (null (cl-set-difference keys1 keys2))
              (null (cl-set-difference keys2 keys1))
              (cl-every (lambda (entry1)
                          (let* ((key (car entry1))
                                 (val1 (cdr entry1))
                                 (val2 (plist-get (cdr b) key)))
                            (if (km2? val1)
                                (km2/eq val1 val2)
                              (equal val1 val2))))
                        (km2/entries a))))))

(defun km2/into (m entries)
  "Add some ENTRIES to M (plist/keyword-map)."
  (cl-reduce (lambda (m entry)
               (km2 (plist-put (cdr m)
                              (car entry)
                              (cdr entry))))
             entries
             :initial-value (copy-tree m)))

(defun km2/merge (&rest kms)
  "Merge several KMS together."
  (cl-reduce (lambda (ret x)
               (km2/into ret (km2/entries x)))
             kms
             :initial-value (km2 ())))

(defun km2/merge-with (f a b)
  "Merge A and B using F to merge values."
  (km2/into a
            (mapcar (lambda (entry)
                      (let* ((k (car entry))
                             (v2 (cdr entry))
                             (v1 (plist-get (cdr a) k))
                             (v (if v1 (funcall f v1 v2) v2)))
                        (cons k v)))
                    (km2/entries b))))

(km2/eq (km2/merge-with #'+
                      (km2 :a 1 :b 2)
                      (km2 :b 3 :c 4))
       (km2 :a 1 :b 5 :c 4))

(defun km2/map (m f)
  "Map F over M. F takes and return an entry (a cons of keyword and value)."
  (km2/into
   ()
   (mapcar f (km2/entries m))))

(defun km2/map-keys (m f)
  "Map F over keys of M. F takes and return a keyword."
  (km2/map m (lambda (e) (cons (funcall f (car e)) (cdr e)))))

(defun km2/map-vals (m f)
  "Map F over values of M."
  (km2/map m (lambda (e) (cons (car e) (funcall f (cdr e))))))

(defun km2/filter (m f)
  "Remove all entries of M for which F return nil or false."
  (let ((entries (km2/entries m)))
    (km2/into
     ()
     (cl-remove-if-not f entries))))

(defun km2/remove (m f)
  "Remove all entries of M for which F return truthy value."
  (let ((entries (km2/entries m)))
    (km2/into
     ()
     (cl-remove-if f entries))))

(defun km2/get-in (m path)
  "Get the value at PATH in the nested keyword map M."
  (cond ((null path) m)
        ((km2? m) (km2/get-in (plist-get (cdr m) (car path))
                            (cdr path)))))

(defun km2/get (m at)
  "Get the value at AT in the keyword map M.
If AT is a list, get the value in a nested map."
  (cond ((listp at) (km2/get-in m at))
        ((vectorp at) (km2/get-in m (append at ())))
        ((keywordp at) (plist-get (cdr m) at))))

(defun km2/contains? (m path)
  "Return t if PATH exist in M."
  (cond ((null path) t)
        ((keywordp path)
         (plist-member (cdr m) path))
        ((listp m)
         (and (plist-member (cdr m) (car path))
              (km2/contains? (plist-get (cdr m) (car path))
                            (cdr path))))))

(defun km2/put-in (m path v)
  "Associate PATH in the nested keyword map M with value V."
  (cond ((null path) v)
        ((km2? m) (let ((p1 (car path)))
                   (km2 (plist-put (cdr m) p1
                                  (km2/put-in (or (plist-get (cdr m) p1)
                                                 (km2 ()))
                                             (cdr path)
                                             v)))))))

(defun km2/put1 (m at v)
  "Put value V at AT in a copy of keyword map M.
If AT is a list, put the value in a nested map."
  (km2/put-in m
             (cond ((listp at) at)
                   ((vectorp at) (append at ()))
                   ((keywordp at) (list at)))
             v))

(defun km2/put (m &rest xs)
  "Associates keys with values in keyword map M using XS."
  (cl-reduce (lambda (m entry)
               (km2/put1 m (car entry) (cadr entry)))
             (sq/partition 2 2 xs)
             :initial-value (copy-tree m)))

(defun km2/upd-in (m path f)
  "Update the value at PATH in the nested keyword map M by applying function F."
  (cond ((null path) (funcall f m))
        ((km2? m) (let ((p1 (car path)))
                   (km2 (plist-put (cdr m) p1
                                  (km2/upd-in (or (plist-get (cdr m) p1)
                                                 (and (cdr path) (km2 ())))
                                             (cdr path)
                                             f)))))))

(defun km2/upd1 (m at f)
  "Update value at AT in a copy of keyword map M by applying function F.
If AT is a list, update the value in a nested map."
  (km2/upd-in m
             (cond ((listp at) at)
                   ((vectorp at) (append at ()))
                   ((keywordp at) (list at)))
             f))

(defun km2/upd (m &rest xs)
  "Update the keyword map M using XS.
XS is a list alternating paths and update-fns."
  (cl-reduce (lambda (m entry)
               (km2/upd1 m (car entry) (cadr entry)))
             (sq/partition 2 2 xs)
             :initial-value (copy-tree m)))

(defun km2/all-paths (m)
  "Transform M into an alist of path -> value."
  (cl-reduce (lambda (ret entry)
                 (let ((k (car entry))
                       (v (cdr entry)))
                   (append ret
                           (if (km2? v)
                               (mapcar (lambda (e)
                                         (cons (cons k (car e))
                                               (cdr e)))
                                       (km2/all-paths v))
                             (list (cons (list k) v))))))
             (km2/entries m)
             :initial-value ()))

(defun km2/select-paths* (m paths)
  "Aggregate all given PATHS with their corresponding value in M."
  (let* ((path-value-pairs (mapcar (lambda (path) (cons path (km2/get m path))) paths)))
    ;; (print path-value-pairs)
    (cl-reduce (lambda (m e) (km2/put m (car e) (cdr e))) path-value-pairs
               :initial-value (km2 ()))))

(defun km2/select-paths (m &rest paths)
  "Aggregate all given PATHS with their corresponding value in M."
  (km2/select-paths* m paths))

(defun km2/parse-free-form (xs)
  "Parse arguments XS to a serie of entries."
  (if (consp xs)
      (let ((x (car xs)))
        (cond ((keywordp x)
               (cons (cons x (cadr xs))
                     (km2/parse-free-form (cddr xs))))
              ((symbolp x)
               (cons (cons (intern (concat ":" (symbol-name x)))
                           x)
                     (km2/parse-free-form (cdr xs))))
              (t (error "Invalid km free form args"))))))

(defmacro km2q (&rest xs)
  "Build a km using free form arguments XS."
  (km2/into () (km2/parse-free-form xs)))

(defun km2/test ()
  "Run some assertions for the keyword map functions."
  (cl-assert
   (and (and (km2? (km2 ()))
             (km2? (km2 :e 2 :d 4)))
        (not (or (km2? (km2 :e 2 :d 4 90))
                 (km2? 2)
                 (km2? (list 4))))))

  (cl-assert
   (and (km2/contains? () ())
        (km2/contains? (km2 :a (km2 :b 1 :c 2) :d 3) '(:a :b))
        (not (km2/contains? (km2 :a (km2 :b 1 :c 2) :d 3) '(:a :e)))
        (km2/contains? (km2 :a (km2 :b 1 :c 2) :d 3) '(:d))
        (not (km2/contains? (km2 :a (km2 :b 1 :c 2) :d 3) '(:f)))))

  (cl-assert
   (and (eq (km2/get (km2 :a (km2 :b 45))
                     (list :a :b))
            45)

        (eq (km2/get (km2 :a 2)
                     :a)
            2)))

  (cl-assert
   (and (equal (km2/put (km2 :a 1 :b (km2 :c 3))
                        [:b :c]
                        78)
               (km2 :a 1 :b (km2 :c 78)))

        (equal (km2/put-in (km2 ())
                           (list :a :b :c)
                           3)
               (km2 :a (km2 :b (km2 :c 3))))

        (equal (km2/put (km2 :a 1 :b (km2 :c 3))
                        (list :b :d :e)
                        78)
               (km2 :a 1 :b (km2 :c 3 :d (km2 :e 78))))))

  (cl-assert
   (and (equal (km2/upd (km2 :a 1) :a (lambda (x) (+ x 1)))
               (km2 :a 2))
        (equal (km2/upd (km2 :a 1 :b (km2 :c 0))
                        [:b :c]
                        (lambda (x) (+ x 1)))
               (km2 :a 1 :b (km2 :c 1)))
        (equal (km2/upd (km2 :a 1)
                        (list :b :c)
                        (lambda (x) (or x 32)))
               (km2 :a 1 :b (km2 :c 32)))))

  (cl-assert
   (equal (km2/into (km2 :a 2)
                    (km2/entries (km2 :b 2 :c 4)))
          (km2 :a 2 :b 2 :c 4)))

  (progn :km2/merge-with
         (cl-assert
          (km2/eq (km2/merge-with #'+
                                  (km2 :a 1 :b 2)
                                  (km2 :b 3 :c 4))
                  (km2 :a 1 :b 5 :c 4)))

         (cl-assert
          (km2/eq (km2/merge-with #'max
                                  (km2 :x 7 :y 8 :z 9)
                                  (km2 :x 10 :y 0 :w 5))
                  (km2 :x 10 :y 8 :z 9 :w 5)))

         (cl-assert
          (km2/eq (km2/merge-with #'cons
                                  (km2 :x 9 :p 3)
                                  (km2 :p (list 1 2)))
                  (km2 :p (list 3 1 2) :x 9))))

  (cl-assert
   (equal (km2/all-paths (km2 :a 2
                              :b (km2 :c 3 :d 6)
                              :d 6
                              :e (km2 :f (km2 :g 4))))
          '(((:a) . 2)
            ((:b :c) . 3)
            ((:b :d) . 6)
            ((:d) . 6) ((:e :f :g) . 4))))

  (cl-assert
   (let ((a 1)(b 2))
     (equal (km2q a b)
            (km2 :a a :b b))))

  (cl-assert
   (and (equal (km2/parse-free-form '(a b :c 34 :d (pouet) f))
               '((:a . a) (:b . b) (:c . 34) (:d pouet) (:f . f)))

        '(equal (should-error (km2/parse-free-form '((+ 1 2) :d (pouet))))
          '(error "Invalid km2 free form args"))))

  (cl-assert
   (equal (km2/filter (km2 :a 1 :b -1)
                      (lambda (e) (> (cdr e) 0)))
          (km2 :a 1)))

  (cl-assert
   (equal (km2/remove (km2 :a 1 :b -1)
                      (lambda (e) (> (cdr e) 0)))
          (km2 :b -1))))

(km2/test)

(provide 'km2)
;;; km2.el ends here.
