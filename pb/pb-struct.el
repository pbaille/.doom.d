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
    (pb_if [(person a b c) '(not-person x z y)]
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
                                    (address street city))))
             (employee "Pierre"
                       43
                       (address
                        "821 Chemin de la chèvre d'or"
                        "Biot"))]
      (list x y street city a))
    '("Pierre"
      43
      "821 Chemin de la chèvre d'or"
      "Biot"
      (address "821 Chemin de la chèvre d'or" "Biot")))))

(pb-struct_run-tests)

(defmacro pb-struct_deftag (tag &optional content-pred)
  "Define a cons structure tagged with TAG (which will be the car of the cons cell).
This macro defines:
- A constructor function named TAG that takes a value and returns a cons with TAG as car and the value as cdr
- A predicate function named TAG? that checks if a value is a properly tagged cons with TAG
- Support for using the tag in pattern matching via pb-destructure

If CONTENT-PRED is provided, it will be used to validate the content before creating the tagged value.
When the predicate returns nil, the constructor will also return nil.

Example:
  (pb-struct_deftag maybe)
  (maybe 42)         ; => (maybe . 42)
  (maybe? (maybe 42)) ; => t
  (pb_let [(maybe x) (maybe 42)] x) ; => 42

With validation:
  (pb-struct_deftag numbox #'numberp)
  (numbox 1)         ; => (numbox . 1)
  (numbox \"hello\")  ; => nil (validation failed)"
  (let ((pred-sym (pb_join-symbol (list tag "?"))))
    `(progn
       (defun ,pred-sym (x)
         ,(format "Return X if it is a %s tagged value." (pb_name tag))
         (and (consp x)
              (eq ',tag (car x))
              ,(if content-pred
                   `(funcall ,content-pred (cdr x))
                 t)
              x))

       (defun ,tag (x &rest xs)
         ,(format "Create a new %s tagged value." (pb_name tag))
         (let ((x (if xs (cons x xs) x)))
           ,(if content-pred
                `(when (funcall ,content-pred x)
                   (cons ',tag x))
              `(cons ',tag x))))

       (pb-destructure_extend ',tag
                              (lambda (args seed)
                                (pb-destructure (car args)
                                                (list 'cdr (list ',pred-sym seed))))))))

(defun pb-struct_deftag-tests ()
  ;; Test basic tag functionality with 'maybe' tag
  (pb-struct_deftag maybe)

  (maybe 1)
  (maybe 1 2)

  (cl-assert
   (and
    (pb_eq (maybe 42) '(maybe . 42) (cons 'maybe 42))
    (maybe? (maybe 42))
    (equal (maybe (list 1 2))
           (maybe 1 2))
    (null (maybe? '(other . 42)))
    (pb_eq (pb_let [(maybe x) (maybe 42)] x)
           (pb_let [(maybe x) (cons 'maybe 42)] x)
           (pb_if [(maybe x) (maybe 42)]
                  x
                  :not-maybe)
           42)
    (equal (pb_if [(maybe x) '(other . 42)]
                  x
                  :not-maybe)
           :not-maybe)))

  ;; Test tag with numeric validation
  (pb-struct_deftag numbox #'numberp)

  (cl-assert
   (and
    (pb_eq (numbox 1) (cons 'numbox 1))
    (null (numbox 'io))
    (equal 1 (pb_let [(numbox x) (numbox 1)] x))
    (null (pb_let [(numbox x) (cons 'numbox 'io)] x)))))

(pb-struct_deftag-tests)

(pb_comment
 (pb-struct_deftag km2)

 (km2? (km2 :a 1 :b 2 :c (km2 :i 4))))

(provide 'pb-struct)
