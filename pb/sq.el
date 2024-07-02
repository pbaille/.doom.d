;;; sq.el --- sequence utils -*- lexical-binding: t; -*-

;; Author: Pierre Baille
;; URL: https://github.com/pbaille
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; sequence utils.

;;; Code:

(require 'cl-lib)

(defun sq_split (index seq)
  "Split the sequence SEQ at the given INDEX into two subsequences.

INDEX is the position at which to split SEQ.
SEQ is the sequence to split.

Returns a list of two subsequences:
the first from the beginning of SEQ to INDEX (exclusive)
and the second from INDEX to the end of SEQ."
  (list (cl-subseq seq 0 index) (cl-subseq seq index)))

(defun sq_partition (size step seq)
  "Partition SEQ into sublists of SIZE elements, moving by STEP each time.

SIZE is the number of elements in each sublist.
STEP is the number of elements to move forward for each new sublist.
SEQ is the sequence to partition.

Returns a list of sublists, each of SIZE elements,
until the end of SEQ is reached."
  (cl-loop for start from 0 to (- (length seq) step) by step
           if (<= (+ start size) (length seq))
           collect (cl-subseq seq start (+ start size))))

(defun sq_range (start end &optional step)
  "Generate a list of numbers from START to END, incrementing by STEP.

START is the beginning of the range.
END is the upper limit of the range (exclusive).
STEP is the optional increment value (defaults to 1).

Returns a list of numbers from START up to but not including END."
  (cl-loop for i from start below end by (or step 1)
           collect i))

(defun sq_butlast (lst)
  "Return a new list containing all elements of LST except the last one."
  (cl-subseq lst 0 (- (length lst) 1)))

(defun sq_last (lst)
  "Return the last element of LST."
  (car-safe (reverse lst)))

(defun sq_interleave (list1 list2)
  "Interleave elements of LIST1 and LIST2."
  (cl-loop for e1 in list1 and e2 in list2
           append (if e1 (list e1) '())
           append (if e2 (list e2) '())))

(defun sq_interpose (lst sep)
  "Interpose SEP between elements of LST."
  (if (cadr lst)
      (append (list (car lst) sep)
              (sq_interpose (cdr lst) sep))
    lst))

(defun sq_take-strict (lst n)
  "Produce a list containing the N first elements of LST.
Returns nil if LST is shorter than N."
  (let ((taken (take n lst)))
    (if (= n (length taken))
        taken)))

(defun sq_drop (lst n)
  "Drop the N first elements of LST."
  (nthcdr n lst))

(defun sq_put (lst idx v)
  "Put V at IDX in LST.
returns nil if IDX is out of bounds, except if it is equal to length,
in this case V is added at the end of the LST."
  (if-let ((head (sq_take-strict lst idx)))
      (append head (cons v (sq_drop lst (+ 1 idx))))))

(defun sq_repeat (n x)
  "Produce a list containing N times X."
  (cl-loop for i from 1 to n collect x))

(defun sq_join (xs)
  "Concat several lists (XS) together."
  (apply #'append xs))

(defun sq_test ()
  "Test."
  (cl-assert
   (and (equal (sq_partition 2 2 '(1 2 3 4 5 6 7))
               '((1 2) (3 4) (5 6)))
        (equal (sq_partition 2 1 '(1 2 3 4))
               '((1 2) (2 3) (3 4)))))

  (cl-assert
   (equal (sq_range 0 10 2)
          '(0 2 4 6 8))

   (equal (sq_range 0 10)
          '(0 1 2 3 4 5 6 7 8 9)))

  (cl-assert
   (equal (sq_split 3 '(1 2 3 4 5))
          '((1 2 3)(4 5))))

  (cl-assert
   (equal (sq_butlast (sq_range 0 10))
          (sq_range 0 9)))

  (cl-assert
   (and (equal (sq_interleave '(a b c) '(x y z))
               '(a x b y c z))
        (equal (sq_interleave '(a b) '(x y z))
               (sq_interleave '(a b c) '(x y)))))

  (cl-assert
   (equal (sq_interpose (list 1 2 3 4) :a)
          '(1 :a 2 :a 3 :a 4)))

  (cl-assert
   (and (equal (sq_put (list 1 2 3 4) 2 9)
               (list 1 2 9 4))

        (equal (sq_put (list 1 2 3 4) 4 10)
               (list 1 2 3 4 10))

        (not (sq_put (list 1 2 3 4) 10 'nop))

        (not (sq_put (list 1 2 3 4) -1 'nop))

        (equal (sq_drop (list 1 2 3 4) 2)
               (list 3 4))

        (equal (sq_drop (list 1 2 3 4) 0)
               (list 1 2 3 4))

        (equal (sq_drop (list 1 2 3 4) -1)
               (list 1 2 3 4))

        (not (sq_drop (list 1 2 3 4) 12)))))

(sq_test)

(provide 'sq)
;;; sq.el ends here.
