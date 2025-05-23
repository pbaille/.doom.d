;;; sq.el --- sequence utils -*- lexical-binding: t; -*-

;; Author: Pierre Baille
;; URL: https://github.com/pbaille
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; sequence utils.

;;; Code:

(require 'cl-lib)

(defun sq/split (index seq)
  "Split the sequence SEQ at the given INDEX into two subsequences.

INDEX is the position at which to split SEQ.
SEQ is the sequence to split.

Returns a list of two subsequences:
the first from the beginning of SEQ to INDEX (exclusive)
and the second from INDEX to the end of SEQ."
  (list (cl-subseq seq 0 index) (cl-subseq seq index)))

(defun sq/partition (size step seq)
  "Partition SEQ into sublists of SIZE elements, moving by STEP each time.

SIZE is the number of elements in each sublist.
STEP is the number of elements to move forward for each new sublist.
SEQ is the sequence to partition.

Returns a list of sublists, each of SIZE elements,
until the end of SEQ is reached."
  (cl-loop for start from 0 to (- (length seq) step) by step
           if (<= (+ start size) (length seq))
           collect (cl-subseq seq start (+ start size))))

(defun sq/range (start end &optional step)
  "Generate a list of numbers from START to END, incrementing by STEP.

START is the beginning of the range.
END is the upper limit of the range (exclusive).
STEP is the optional increment value (defaults to 1).

Returns a list of numbers from START up to but not including END."
  (cl-loop for i from start below end by (or step 1)
           collect i))

(defun sq/butlast (lst)
  "Return a new list containing all elements of LST except the last one."
  (cl-subseq lst 0 (- (length lst) 1)))

(defun sq/last (lst)
  "Return the last element of LST."
  (car-safe (reverse lst)))

(defun sq/interleave (list1 list2)
  "Interleave elements of LIST1 and LIST2."
  (cl-loop for e1 in list1 and e2 in list2
           append (if e1 (list e1) '())
           append (if e2 (list e2) '())))

(defun sq/interpose (lst sep)
  "Interpose SEP between elements of LST."
  (if (cadr lst)
      (append (list (car lst) sep)
              (sq/interpose (cdr lst) sep))
    lst))

(defun sq/take-strict (lst n)
  "Produce a list containing the N first elements of LST.
Returns nil if LST is shorter than N."
  (let ((taken (take n lst)))
    (if (= n (length taken))
        taken)))

(defun sq/drop (lst n)
  "Drop the N first elements of LST."
  (nthcdr n lst))

(defun sq/put (lst idx v)
  "Put V at IDX in LST.
returns nil if IDX is out of bounds, except if it is equal to length,
in this case V is added at the end of the LST."
  (if-let ((head (sq/take-strict lst idx)))
      (append head (cons v (sq/drop lst (+ 1 idx))))))

(defun sq/repeat (n x)
  "Produce a list containing N times X."
  (cl-loop for i from 1 to n collect x))

(defun sq/join (xs)
  "Concat several lists (XS) together."
  (apply #'append xs))

(defun sq/find (xs f)
  "Return the first element of XS that satisfies F."
  (if (consp xs)
      (let ((x (car xs)))
        (or (and (funcall f x) x)
            (sq/find (cdr xs) f)))))

(defun sq/index-of (xs x)
  "Return the first index of XS where element is equal to X."
  (if-let ((found (sq/find (seq-mapn #'cons (sq/range 0 (length xs)) xs)
                           (lambda (e) (equal x (cdr e))))))
      (car found)))

(defun sq/group-by (function sequence)
  "Group elements in SEQUENCE by result of FUNCTION applied to them.
   Returns an alist of (VALUE . ELEMENTS) where VALUE is the result of
   applying FUNCTION to an element of SEQUENCE, and ELEMENTS is a list
   of all elements for which FUNCTION returns that value."
  (let ((groups (make-hash-table :test 'equal)))
    (seq-doseq (elt sequence)
      (let ((key (funcall function elt)))
        (puthash key (cons elt (gethash key groups nil)) groups)))
    (let (result)
      (maphash (lambda (k v) (push (cons k (nreverse v)) result)) groups)
      result)))

(defun sq/count-by (function sequence)
  "Count elements in SEQUENCE grouped by result of FUNCTION applied to them.
Returns an alist of (VALUE . COUNT) where VALUE is the result of
applying FUNCTION to elements of SEQUENCE, and COUNT is the number
of elements for which FUNCTION returns that value."
  (let ((counts (make-hash-table :test 'equal)))
    (seq-doseq (elt sequence)
      (let ((key (funcall function elt)))
        (puthash key (1+ (gethash key counts 0)) counts)))
    (let (result)
      (maphash (lambda (k v) (push (cons k v) result)) counts)
      result)))

(defun sq/test ()
  "Test."
  (cl-assert
   (and (equal (sq/partition 2 2 '(1 2 3 4 5 6 7))
               '((1 2) (3 4) (5 6)))
        (equal (sq/partition 2 1 '(1 2 3 4))
               '((1 2) (2 3) (3 4)))))

  (cl-assert
   (equal (sq/range 0 10 2)
          '(0 2 4 6 8))

   (equal (sq/range 0 10)
          '(0 1 2 3 4 5 6 7 8 9)))

  (cl-assert
   (equal (sq/split 3 '(1 2 3 4 5))
          '((1 2 3)(4 5))))

  (cl-assert
   (equal (sq/butlast (sq/range 0 10))
          (sq/range 0 9)))

  (cl-assert
   (and (equal (sq/interleave '(a b c) '(x y z))
               '(a x b y c z))
        (equal (sq/interleave '(a b) '(x y z))
               (sq/interleave '(a b c) '(x y)))))

  (cl-assert
   (equal (sq/interpose (list 1 2 3 4) :a)
          '(1 :a 2 :a 3 :a 4)))

  (cl-assert
   (and (equal (sq/put (list 1 2 3 4) 2 9)
               (list 1 2 9 4))

        (equal (sq/put (list 1 2 3 4) 4 10)
               (list 1 2 3 4 10))

        (not (sq/put (list 1 2 3 4) 10 'nop))

        (not (sq/put (list 1 2 3 4) -1 'nop))

        (equal (sq/drop (list 1 2 3 4) 2)
               (list 3 4))

        (equal (sq/drop (list 1 2 3 4) 10)
               (list))

        (equal (sq/drop (list 1 2 3 4) 0)
               (list 1 2 3 4))

        (equal (sq/drop (list 1 2 3 4) -1)
               (list 1 2 3 4))

        (not (sq/drop (list 1 2 3 4) 12))))
  (cl-assert
   (and (equal (sq/find (list :a 's 3 :b "er" 'c)
                        #'stringp)
               "er")

        (equal (sq/index-of (list :a 's 3 :b "er" 'c "er")
                            "er")
               4)

        (not (sq/index-of (list :a 's 3 :b "er" 'c "er")
                          "ert"))))

  (cl-assert
   (and (equal (sq/group-by (lambda (s) (substring s 0 1))
                            '("apple" "banana" "apricot" "blueberry" "cherry" "avocado"))
               '(("c" "cherry") ("b" "banana" "blueberry") ("a" "apple" "apricot" "avocado")))
        (equal (sq/group-by (lambda (n) (if (zerop (mod n 2)) 'even 'odd))
                            '(1 2 3 4 5 6 7 8 9 10))
               '((even 2 4 6 8 10) (odd 1 3 5 7 9)))

        (equal (sq/group-by #'length '("a" "bb" "ccc" "dd" "eee" "f" "ggg"))
               '((3 "ccc" "eee" "ggg") (2 "bb" "dd") (1 "a" "f"))))))

(sq/test)

(provide 'sq)
;;; sq.el ends here.
