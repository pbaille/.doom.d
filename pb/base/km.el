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

(defun km* (xs)
  "Build a keyword map from XS.
Throws an error if XS does not form a valid keyword map."
  (if (km? xs)
      xs
    (error (format "Bad argument to km: %s" xs))))

(defun km (&rest xs)
  "Build a keyword map from XS.
Throws an error if XS does not form a valid keyword map."
  (km* xs))

(defun km/entries (m)
  "Return the entries of the keyword map M as pairs (key . value)."
  (if (consp m)
      (cons (cons (car m) (cadr m))
            (km/entries (cddr m)))))

(defun km/eq (km1 km2)
  "Check if two keyword maps KM1 and KM2 are equal.
Keyword maps are considered equal if they contain the same keys
with the same associated values."
  (and (km? km1)
       (km? km2)
       (let ((keys1 (km/keys km1))
             (keys2 (km/keys km2)))
         (and (null (cl-set-difference keys1 keys2))
              (null (cl-set-difference keys2 keys1))
              (cl-every (lambda (entry1)
                          (let* ((key (car entry1))
                                 (val1 (cdr entry1))
                                 (val2 (plist-get km2 key)))
                            (if (km? val1)
                                (km/eq val1 val2)
                              (equal val1 val2))))
                        (km/entries km1))))))

(defun km/into (m entries)
  "Add some ENTRIES to M (plist/keyword-map)."
  (cl-reduce (lambda (m entry)
               (plist-put m (car entry) (cdr entry)))
             entries
             :initial-value (copy-tree m)))

(defun km/merge (&rest kms)
  "Merge several KMS together."
  (cl-reduce (lambda (ret x) (km/into ret (km/entries x)))
             kms
             :initial-value ()))

(defun km/merge-with (f km1 km2)
  "Merge KM1 and KM2 using F to merge values."
  (km/into km1
           (mapcar (lambda (entry)
                     (let* ((k (car entry))
                            (v2 (cdr entry))
                            (v1 (plist-get km1 k))
                            (v (if v1 (funcall f v1 v2) v2)))
                       (cons k v)))
                   (km/entries km2))))

(defun km/map (m f)
  "Map F over M. F takes and return an entry (a cons of keyword and value)."
  (km/into
   ()
   (mapcar f (km/entries m))))

(defun km/map-keys (m f)
  "Map F over keys of M. F takes and return a keyword."
  (km/map m (lambda (e) (cons (funcall f (car e)) (cdr e)))))

(defun km/map-vals (m f)
  "Map F over values of M."
  (km/map m (lambda (e) (cons (car e) (funcall f (cdr e))))))

(defun km/filter (m f)
  "Remove all entries of M for which F return nil or false."
  (let ((entries (km/entries m)))
    (km/into
     ()
     (cl-remove-if-not f entries))))

(defun km/remove (m f)
  "Remove all entries of M for which F return truthy value."
  (let ((entries (km/entries m)))
    (km/into
     ()
     (cl-remove-if f entries))))

(defun km/keys (m)
  "Return the keys of the keyword map M."
  (and (km? m)
       (mapcar #'car (km/entries m))))

(defun km/vals (m)
  "Return the values of the keyword map M."
  (and (km? m)
       (mapcar #'cdr (km/entries m))))

(defun km/get-in (m path)
  "Get the value at PATH in the nested keyword map M."
  (if (and m path)
      (km/get-in (plist-get m (car path)) (cdr path))
    m))

(defun km/get (m at)
  "Get the value at AT in the keyword map M.
If AT is a list, get the value in a nested map."
  (cond ((listp at) (km/get-in m at))
        ((vectorp at) (km/get-in m (append at ())))
        ((keywordp at) (plist-get m at))))

(defun km/contains? (m path)
  "Return t if PATH exist in M."
  (cond ((not path) t)
        ((keywordp path)
         (km/contains? m (list path)))
        ((listp m)
         (and (plist-member m (car path))
              (km/contains? (plist-get m (car path)) (cdr path))))))

(defun km/put-in (m path v)
  "Associate PATH in the nested keyword map M with value V."
  (if path
      (let ((p1 (car path)))
        (if (km? m)
            (plist-put m p1 (km/put-in (plist-get m p1) (cdr path) v))))
    v))

(defun km/put1 (m at v)
  "Put value V at AT in a copy of keyword map M.
If AT is a list, put the value in a nested map."
  (km/put-in m
             (cond ((listp at) at)
                   ((vectorp at) (append at ()))
                   ((keywordp at) (list at)))
             v))

(defun km/put (m &rest xs)
  "Associates keys with values in keyword map M using XS."
  (cl-reduce (lambda (m entry)
               (km/put1 m (car entry) (cadr entry)))
             (sq/partition 2 2 xs)
             :initial-value (copy-tree m)))

(defun km/upd-in (m path f)
  "Update the value at PATH in the nested keyword map M by applying function F."
  (if path
      (let ((p1 (car path)))
        (if (km? m)
            (plist-put m p1 (km/upd-in (plist-get m p1) (cdr path) f))))
    (funcall f m)))

(defun km/upd1 (m at f)
  "Update value at AT in a copy of keyword map M by applying function F.
If AT is a list, update the value in a nested map."
  (km/upd-in m
             (cond ((listp at) at)
                   ((vectorp at) (append at ()))
                   ((keywordp at) (list at)))
             f))

(defun km/upd (m &rest xs)
  "Update the keyword map M using XS.
XS is a list alternating paths and update-fns."
  (cl-reduce (lambda (m entry)
               (km/upd1 m (car entry) (cadr entry)))
             (sq/partition 2 2 xs)
             :initial-value (copy-tree m)))

(defun km/all-paths (m)
  "Transform M into an alist of path -> value."
  (cl-reduce (lambda (ret entry)
                 (let ((k (car entry))
                       (v (cdr entry)))
                   (append ret
                           (if (km? v)
                               (mapcar (lambda (e)
                                         (cons (cons k (car e))
                                               (cdr e)))
                                       (km/all-paths v))
                             (list (cons (list k) v))))))
             (km/entries m)
             :initial-value ()))

(defun km/select-paths* (m paths)
  "Aggregate all given PATHS with their corresponding value in M."
  (let* ((path-value-pairs (mapcar (lambda (path) (cons path (km/get m path))) paths)))
    ;; (print path-value-pairs)
    (cl-reduce (lambda (m e) (km/put m (car e) (cdr e))) path-value-pairs
               :initial-value ())))

(defun km/select-paths (m &rest paths)
  "Aggregate all given PATHS with their corresponding value in M."
  (km/select-paths* m paths))

(progn :pretty-str

       (defun km/pp (m &optional indentation-lvl)
         "Get a pretty string representing M. Entries are stacked vertically, and nested kms are processed recursively and indented.
          Also handles lists of keyword maps."
         (let ((indent (make-string (or indentation-lvl 0) ?\s)))
           (cond
            ((null m) "nil")
            ((km? m)
             (let ((entries (km/entries m)))
               (if (null entries)
                   "(km)"
                 (concat "(km "
                         (mapconcat
                          (lambda (entry)
                            (let* ((k (car entry))
                                   (v (cdr entry))
                                   (v-str (km/pp v
                                                 (+ (or indentation-lvl 0)
                                                    4
                                                    (1+ (length (symbol-name k)))))))
                              (format "%s %s" k v-str)))
                          entries
                          (concat "\n" indent "    "))
                         ")"))))
            ((listp m)
             (concat "("
                     (mapconcat (lambda (km) (km/pp km (1+ (or indentation-lvl 0))))
                                m
                                (concat "\n " indent))
                     ")"))
            (t (prin1-to-string m)))))

       [:tests
        (km :a 1 :b 2 :tobu (km :c1 3 :c2 5)
            :b "oiu"
            :tobu (km :sipos (km :fun 3) :c2 5))

        (message (concat "\n"
                         (km/pp (km :a 1 :b 2 :tobu (km :c1 3 :c2 5)
                                    :b "oiu"
                                    :tobu (km :sipos (km :fun 3) :c2 5)))))

        (pb-prompt/describe-path "~/.doom.d/pb")

        (km :name "pb"
            :type "dir"
            :children (km :archived (km :name "archived"
                                        :type "dir"
                                        :children (km :pb-lisp.el (km :name "pb-lisp.el"
                                                                      :type "file"
                                                                      :content :pouet)
                                                      :reaper.el (km :name "reaper.el"
                                                                     :type "file"
                                                                     :content :pouet)))))])

(defmacro km/let (binding &rest body)
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

(defmacro km/lambda (ks &rest body)
  "Define a function that takes a keyword map as its sole argument.
KS specifies the expected keys, and BODY is the code to execute."
  (let ((seed (gensym)))
    `(lambda (,seed)
       (km/let (,ks ,seed)
                ,@body))))

(defmacro km/defun (name ks &rest body)
  "Define a named function that takes a keyword map as its sole argument.
NAME is the function name, KS specifies the expected keys,
and BODY is the code to execute."
  (let ((seed (gensym)))
    `(defun ,name (,seed)
       (km/let (,ks ,seed)
               ,@body))))

(defun km/parse-free-form (xs)
  "Parse arguments XS to a serie of entries."
  (if (consp xs)
      (let ((x (car xs)))
        (cond ((keywordp x)
               (cons (cons x (cadr xs))
                     (km/parse-free-form (cddr xs))))
              ((symbolp x)
               (cons (cons (intern (concat ":" (symbol-name x)))
                           x)
                     (km/parse-free-form (cdr xs))))
              (t (error "Invalid km free form args"))))))

(defmacro kmq (&rest xs)
  "Build a km using free form arguments XS."
  (cons 'list (km/into () (km/parse-free-form xs))))

(defun km/test ()
  "Run some assertions for the keyword map functions."
  (cl-assert
   (and (and (km? ())
             (km? (list :e 2 :d 4)))
        (not (or (km? (list :e 2 :d 4 90))
                 (km? 2)
                 (km? (list 4))))))

  (cl-assert
   (and (km/contains? () ())
        (km/contains? (km :a (km :b 1 :c 2) :d 3) '(:a :b))
        (not (km/contains? (km :a (km :b 1 :c 2) :d 3) '(:a :e)))
        (km/contains? (km :a (km :b 1 :c 2) :d 3) '(:d))
        (not (km/contains? (km :a (km :b 1 :c 2) :d 3) '(:f)))))

  (cl-assert
   (and (eq (km/get (km :a (km :b 45))
                    (list :a :b))
            45)

        (eq (km/get (km :a 2)
                    :a)
            2)))

  (cl-assert
   (and (equal (km/put '(:a 1 :b (:c 3))
                       [:b :c]
                       78)
               '(:a 1 :b (:c 78)))

        (equal (km/put-in () (list :a :b :c) 3)
               '(:a (:b (:c 3))))

        (equal (km/put (km :a 1 :b (km :c 3))
                       (list :b :d :e)
                       78)
               '(:a 1 :b (:c 3 :d (:e 78))))))

  (cl-assert
   (and (equal (km/upd '(:a 1) :a (lambda (x) (+ x 1)))
               '(:a 2))
        (equal (km/upd '(:a 1 :b (:c 0)) [:b :c] (lambda (x) (+ x 1)))
               '(:a 1 :b (:c 1)))
        (equal (km/upd '(:a 1) '(:b :c) (lambda (x) (or x 32)))
               '(:a 1 :b (:c 32)))))

  (cl-assert
   (equal (km/let ((a b) (km :a 1 :b 2))
                  (+ a b))
          3)
   (equal (km/let ((:as m a b) (km :a 1 :b 2))
                  (km/put m :ret (+ a b)))
          (km :a 1 :b 2 :ret 3))
   (equal (funcall (km/lambda (a b) (+ a b))
                   (km :a 1 :b 3))
          4))

  (progn :km/merge-with
         (cl-assert
          (km/eq (km/merge-with #'+ (km :a 1 :b 2)
                                (km :b 3 :c 4))
                 '(:a 1 :b 5 :c 4)))

         (cl-assert
          (km/eq (km/merge-with #'max (km :x 7 :y 8 :z 9)
                                (km :x 10 :y 0 :w 5))
                 '(:x 10 :y 8 :z 9 :w 5)))

         (cl-assert
          (km/eq (km/merge-with #'cons
                                (km :x 9 :p 3)
                                (km :p (list 1 2)))
                 '(:p (3 1 2) :x 9))))

  (cl-assert
   (equal (km/into (km :a 2)
                   (km/entries (km :b 2 :c 4)))
          (km :a 2 :b 2 :c 4)))

  (cl-assert
   (equal (km/all-paths (km :a 2
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
            (km :a a :b b))))

  (cl-assert
   (and (equal (km/parse-free-form '(a b :c 34 :d (pouet) f))
               '((:a . a) (:b . b) (:c . 34) (:d pouet) (:f . f)))

        '(equal (should-error (km/parse-free-form '((+ 1 2) :d (pouet))))
          '(error "Invalid km free form args"))))

  (cl-assert
   (equal (km/filter (km :a 1 :b -1)
                     (lambda (e) (> (cdr e) 0)))
          '(:a 1)))

  (cl-assert
   (equal (km/remove (km :a 1 :b -1)
                     (lambda (e) (> (cdr e) 0)))
          '(:b -1))))

(km/test)

(provide 'km)
;;; km.el ends here.
