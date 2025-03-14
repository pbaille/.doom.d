;;; pb/xp/structs.el -*- lexical-binding: t; -*-

(require 'pb)

(pb_comment
 :extend-destructure-for-simple-struct
 (eval-when-compile

   (defun pb-destructure_simple-struct-impl (head-sym)
     (lambda (args seed)
       (pb-destructure `(cons _ (list ,@args))
                       seed)))

   (defun pb-destructure_add-simple-struct (sym)
     (pb-destructure_extend sym
                            (pb-destructure_simple-struct-impl sym)))
   (pb-destructure_add-simple-struct 'point))

 (pb_let [(point x y) `(point 1 2)]
     (cl-assert (equal (list 1 2)
                       (list x y)))))

(defmacro pb-struct (name &rest members)
  "Define a struct NAME with given MEMBERS.
A struct is a simple list with the NAME symbol as its car and MEMBERS as cdr.
For each member M, a function NAME.M is defined to access that field.
A predicate function NAME? is also defined to check if a value is an instance of this struct.
The struct type can be used with pb's destructuring facilities.

Example:
  (pb-struct person name age)
  (setq p (person \"John\" 30))
  (person? p)         ; => t
  (person.name p)     ; => \"John\"
  (pb_let [(person name age) p] name) ; => \"John\""
  (let* ((pred-sym (pb_symbol name "?")))
    `(progn
       (defun ,pred-sym (x)
         ,(format "Check if X is a %s struct."
                  (pb_name name))
         (when (listp x)
           (eq ',name (car-safe x))))

       ,@(seq-map-indexed
          (lambda (m i)
            `(defun ,(pb_symbol name "." m) (x)
               ,(format "Get the %s field from the %s struct."
                        (pb_name m)
                        (pb_name name))
               (when (,pred-sym x)
                 (nth ,(1+ i) x))))
          members)

       (defun ,name ,members
         ,(format "Create a new %s struct with the given fields."
                  (pb_name name))
         (list ',name ,@members))

       (pb-destructure_extend ',name
                              (lambda (args seed)
                                (pb-destructure (list 'cons
                                                      (list 'eq '_ '',name)
                                                      (cons 'list args))
                                                seed))))))

(defun pb-struct_run-tests ()

  (pb-struct person name age)

  (pb-destructure '(person a b c) '(x z y))
  (cl-assert
   (equal
    (pb_if [(person a b c) '(x z y)]
           (list a b c)
           :ok)
    :ok))

  (cl-assert
    (equal
     (pb_let [x (person "Pierre" 43)]
         (list (person? x)
               (person.name x)))
     '(t "Pierre")))

  (cl-assert
   (equal
    (pb_let [(person name age) (person "Pierre" 43)]
        (list name age))
    '("Pierre" 43)))

  (pb-struct address street city)
  (pb-struct employee name age address)

  (cl-assert
   (equal
    (pb_let [(as emp
                 (employee x y (and (address? a)
                                    (address ))))
             (employee "Pierre"
                       43
                       (address
                        "821 Chemin de la chèvre d'or"
                        "Biot"))]
        (list x y a))
    '("Pierre" 43
      (address "821 Chemin de la chèvre d'or" "Biot")))))

(pb-struct_run-tests)
