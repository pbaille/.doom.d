;;; pb/vec.el -*- lexical-binding: t; -*-

(defun vec (x)
  "Convert X to vector.
X can be a list or vector. Throws an error for other types."
  (cond ((listp x) (vconcat x))
        ((vectorp x) x)
        (t (error "Cannot convert %S to vector" x))))

(defun vec? (x)
  "Check if X is a vector."
  (vectorp x))

(defun vec_to-list (x)
  (when (vec? x)
    (append x ())))

(defun vec_get (v at)
  "Get value from vector V at position AT.
AT can be an integer index or a sequence (list or vector) of indices
for nested access. Returns nil if the path does not exist.
Negative indices count from the end of the vector (-1 is the last element)."
  (when (vec? v)
    (cond ((null at) v)
          ((integerp at)
           (let ((len (length v))
                 (idx (if (< at 0) (+ (length v) at) at)))
             (when (and (>= idx 0) (< idx len))
               (aref v idx))))
          ((listp at)
           (when-let ((found (vec_get v (car at))))
             (if (cdr at)
                 (vec_get found (cdr at))
               found)))
          ((vec? at) (vec_get v (vec_to-list at)))
          (t (error "Invalid index type: %S" at)))))

(defun vec_put (v at val)
  "Set value in vector V at position AT to VAL.
AT can be an integer index or a sequence (list or vector) of indices
for nested access. Returns a new vector with the updated value.
Negative indices count from the end of the vector (-1 is the last element).
If the path doesn't exist, throws an error."
  (when (vec? v)
    (cond ((null at) val)
          ((integerp at)
           (let* ((len (length v))
                  (idx (if (< at 0) (+ len at) at)))
             (and (>= idx 0)
                  (< idx len)
                  (let ((result (copy-sequence v)))
                    (aset result idx val)
                    result))))
          ((listp at)
           (if (null (cdr at))
               (vec_put v (car at) val)
             (let* ((idx (car at))
                    (rest (cdr at))
                    (sub (vec_put (vec_get v idx) rest val)))
               (if sub
                   (vec_put v idx sub)))))
          ((vec? at) (vec_put v (vec_to-list at) val))
          (t (error "Invalid index type: %S" at)))))

(defun vec_upd (v at f)
  "Update value in vector V at position AT by applying function F.
AT can be an integer index or a sequence (list or vector) of indices
for nested access. Returns a new vector with the updated value.
Negative indices count from the end of the vector (-1 is the last element).
If the path doesn't exist, throws an error."
  (when (vec? v)
    (cond ((null at) (funcall f v))
          ((integerp at)
           (let* ((len (length v))
                  (idx (if (< at 0) (+ len at) at)))
             (and (>= idx 0)
                  (< idx len)
                  (let ((result (copy-sequence v))
                        (val (funcall f (aref v idx))))
                    (aset result idx val)
                    result))))
          ((listp at)
           (if (null (cdr at))
               (vec_upd v (car at) f)
             (let* ((idx (car at))
                    (rest (cdr at))
                    (sub (vec_upd (vec_get v idx) rest f)))
               (if sub
                   (vec_put v idx sub)))))
          ((vec? at) (vec_upd v (vec_to-list at) f))
          (t (error "Invalid index type: %S" at)))))

(defun vec_into (v xs)
  "Add items from XS to the end of vector V.
XS can be a list or vector. Returns a new vector with the items added."
  (when (vec? v)
    (let ((items (cond ((listp xs) xs)
                       ((vectorp xs) (vec_to-list xs))
                       (t (error "Cannot convert %S to vector items" xs)))))
      (vconcat v items))))

(defun vec_conj (v x)
  "Add element X to the end of vector V.
Returns a new vector with X appended. If V is not a vector, throws an error."
  (when (vec? v)
    (vconcat v (list x))))

(defun vec_split (v idx)
  "Split V in 2 at IDX.
Returns a cons cell (left . right) where left contains elements [0..idx-1]
and right contains elements [idx..length-1]. If IDX is negative, it counts
from the end of the vector. Returns nil if V is not a vector or if IDX
is out of bounds."
  (when (vec? v)
    (let* ((len (length v))
           (pos (if (< idx 0) (+ len (1+ idx)) idx)))
      (when (and (>= pos 0) (<= pos len))
        (cons (if (= pos 0)
                  []
                (seq-subseq v 0 pos))
              (if (= pos len)
                  []
                (seq-subseq v pos len)))))))

(defun vec_insert (v idx xs)
  "Insert items from XS into vector V at position IDX.
IDX can be a positive or negative integer. Negative indices count from the end.
XS can be a list or vector. Returns a new vector with the items inserted.
Returns nil if V is not a vector or if IDX is out of bounds."
  (when (vec? v)
    (when-let ((parts (vec_split v idx)))
      (let ((left (car parts))
            (right (cdr parts)))
        (vec_into (vec_into left xs) right)))))

(defun vec_test ()
  "Run some assertions for the vector functions."
  (cl-assert
   (and (vectorp (vec '(1 2 3)))
        (vectorp (vec [1 2 3]))
        (equal (vec '(1 2 3)) [1 2 3])
        (equal (vec [1 2 3]) [1 2 3])))

  (cl-assert
   (and (= (vec_get [1 2 3] 0) 1)
        (= (vec_get [1 2 3] 2) 3)
        (= (vec_get [1 2 3] -1) 3)
        (= (vec_get [1 2 3] -3) 1)
        (equal (vec_get [[1 2] [3 4]] 1) [3 4])
        (= (vec_get [[1 2] [3 4]] '(0 1)) 2)
        (equal (vec_get [[1 2] [3 4]] [1 0])
               3)
        (= (vec_get [[1 2] [[5 6] 7]] '(1 0 1)) 6)
        (equal (vec_get [[1 2] [3 4]] ()) [[1 2] [3 4]])
        (null (vec_get [1 2 3] 5))
        (null (vec_get [1 2 3] -5))
        (null (vec_get [1 2] '(0 5)))
        (null (vec_get [] 0))))

  (cl-assert
   (and (equal (vec_put [1 2 3] 0 5) [5 2 3])
        (equal (vec_put [1 2 3] -1 5) [1 2 5])
        (equal (vec_put [[1 2] [3 4]] 0 [5 6]) [[5 6] [3 4]])
        (equal (vec_put [[1 2] [3 4]] '(0 1) 7) [[1 7] [3 4]])
        (equal (vec_put [[1 2] [3 4]] [1 0] 9) [[1 2] [9 4]])
        (equal (vec_put [[1 2] [[5 6] 7]] '(1 0 1) 8) [[1 2] [[5 8] 7]])
        (null (vec_put [1 2 3] 5 9))
        (null (vec_put [1 2 3] -5 9))
        (null (vec_put [1 2] '(0 5) 3))))

  (cl-assert
   (and (equal (vec_upd [1 2 3] 0 #'1+) [2 2 3])
        (equal (vec_upd [1 2 3] -1 #'1+) [1 2 4])
        (equal (vec_upd [[1 2] [3 4]] 0 (lambda (v) (vec_upd v 1 #'1+))) [[1 3] [3 4]])
        (equal (vec_upd [[1 2] [3 4]] '(0 1) #'1+) [[1 3] [3 4]])
        (equal (vec_upd [[1 2] [3 4]] [1 0] #'1+) [[1 2] [4 4]])
        (equal (vec_upd [[1 2] [[5 6] 7]] '(1 0 1) #'1+) [[1 2] [[5 7] 7]])
        (equal (vec_upd [1 2 3] () #'reverse) [3 2 1])
        (null (vec_upd [1 2 3] 5 #'1+))
        (null (vec_upd [1 2 3] -5 #'1+))
        (null (vec_upd [1 2] '(0 5) #'1+))))

  (cl-assert
   (and (equal (vec_into [1 2 3] [4 5]) [1 2 3 4 5])
        (equal (vec_into [1 2 3] '(4 5)) [1 2 3 4 5])
        (equal (vec_into [1 2] []) [1 2])
        (equal (vec_into [] [1 2 3]) [1 2 3])
        (equal (vec_into [] '()) [])
        (condition-case nil
            (progn (vec_into [1 2 3] 4) nil)
          (error t))))

  (cl-assert
   (and (equal (vec_insert [1 2 3] 1 [4 5]) [1 4 5 2 3])
        (equal (vec_insert [1 2 3] 0 [4 5]) [4 5 1 2 3])
        (equal (vec_insert [1 2 3] 3 [4 5]) [1 2 3 4 5])
        (equal (vec_insert [1 2 3] -1 [4 5]) [1 2 3 4 5])
        (equal (vec_insert [1 2 3] -2 [4 5]) [1 2 4 5 3])
        (equal (vec_insert [1 2 3] 1 '(4 5)) [1 4 5 2 3])
        (equal (vec_insert [] 0 [1 2]) [1 2])
        (null (vec_insert [1 2 3] 4 [4 5]))
        (equal (vec_insert [1 2 3] -4 [4 5]) [4 5 1 2 3])))

  (cl-assert
   (and (equal (vec_split [1 2 3] 1) '([1] . [2 3]))
        (equal (vec_split [1 2 3] 0) '([] . [1 2 3]))
        (equal (vec_split [1 2 3] 3) '([1 2 3] . []))
        (equal (vec_split [1 2 3] -1) '([1 2 3] . []))
        (equal (vec_split [1 2 3] -3) '([1] . [2 3]))
        (equal (vec_split [1 2 3] -4) '([] . [1 2 3]))
        (null (vec_split [1 2 3] 4))
        (equal (vec_split [] 0) '([] . []))))

  (cl-assert
   (and (equal (vec_conj [1 2 3] 4) [1 2 3 4])
        (equal (vec_conj [] 1) [1])
        (equal (vec_conj [1] 2) [1 2])
        (equal (vec_conj [1 2] "a") [1 2 "a"])
        (equal (vec_conj [1 2] [3 4]) [1 2 [3 4]])
        (null (vec_conj nil 1))
        (null (vec_conj "not-a-vector" 1)))))

(vec_test)

(provide 'vec)
