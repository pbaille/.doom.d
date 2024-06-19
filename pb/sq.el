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
  (cl-loop for start from 0 below (length seq) by step
           collect (cl-subseq seq start (min (+ start size) (length seq)))))

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

(defun sq_test ()
  "Test."
  (cl-assert
   (and (equal (sq_partition 2 2 '(1 2 3 4 5 6 7))
               '((1 2) (3 4) (5 6) (7)))
        (equal (sq_partition 2 1 '(1 2 3 4))
               '((1 2) (2 3) (3 4) (4)))))

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
          (sq_range 0 9))))

(sq_test)

(provide 'sq)
;;; sq.el ends here.
