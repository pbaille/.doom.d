;;; pb/sequences.el -*- lexical-binding: t; -*-

(require 'cl-lib)

(defun first (s) (car s))
(defun second (s) (cadr s))
(defun third (s) (caddr s))

(defun sq-split (index seq)
  "Split the sequence SEQ at the given INDEX, returning a list of two subsequences."
  (list (cl-subseq seq 0 index) (cl-subseq seq index)))

(defun sq-partition (size step seq)
  "Partition the sequence SEQ into sub-sequences of SIZE elements,
sliding over the sequence with a step of STEP."
  (cl-loop for start from 0 below (length seq) by step
           collect (cl-subseq seq start (min (+ start size) (length seq)))))

(defun sq-range (start end &optional step)
  "Return a sequence of numbers starting from START (inclusive) and ending at END (exclusive),
with a step size of STEP. If STEP is not specified, it defaults to 1."
  (cl-loop for i from start below end by (or step 1)
           collect i))

(defun sq-butlast (lst)
  "Return a new list containing all elements of LST except the last one."
  (cl-subseq lst 0 (- (length lst) 1)))

(defun test ()
  (cl-assert
   (and (equal (sq-partition 2 2 '(1 2 3 4 5 6 7))
               '((1 2) (3 4) (5 6) (7)))
        (equal (sq-partition 2 1 '(1 2 3 4))
               '((1 2) (2 3) (3 4) (4)))))

  (cl-assert
   (equal (sq-range 0 10 2)
          '(0 2 4 6 8))

   (equal (sq-range 0 10)
          '(0 1 2 3 4 5 6 7 8 9)))

  (cl-assert
   (equal (sq-split 3 '(1 2 3 4 5))
          '((1 2 3)(4 5))))

  (cl-assert
   (equal (sq-butlast (sq-range 0 10))
          (sq-range 0 9))))

(test)

(provide 'sequences)
