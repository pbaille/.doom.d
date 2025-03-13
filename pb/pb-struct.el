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
  "Defines a struct NAME with given MEMBERS.
It is a simple list holding the name symbol as car and members as cdr.
MEMBERS can hold substrctures that will be recursively created"
  (let* ((struct-name (symbol-name name))
         (pred-name (concat struct-name "?"))
         (pred-sym (intern pred-name))
         (constr-sym name))
    `(progn
       (defun ,pred-sym (x)
         ,(format "Check if X is a %s struct." struct-name)
         (and (listp x)
              (eq ',name (car-safe x))))

       ,@(seq-map-indexed (lambda (m i)
                            (let ((name (intern (concat struct-name "." (symbol-name m)))))
                              `(defun ,name (x)
                                 (when (,(intern pred-name) x)
                                   (nth ,(1+ i) x)))))
                          members)

       (defun ,constr-sym ,members
         ,(format "Create a new %s struct with the given fields." struct-name)
         (list ',name ,@members))

       (pb-destructure_extend ',name
                              (lambda (args seed)
                                (pb-destructure `(cons _ (list ,@args))
                                                seed))))))

(defun pb-struct_run-tests ()

  (pb-struct person name age)

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
                 (employee x y (address z u)))
             (employee "Pierre"
                       43
                       (address
                        "821 Chemin de la chèvre d'or"
                        "Biot"))]
        (list
         (employee.address emp)
         (list x y z u)))
    (list
     (address "821 Chemin de la chèvre d'or" "Biot")
     (list "Pierre" 43 "821 Chemin de la chèvre d'or" "Biot")))))

(pb-struct_run-tests)
