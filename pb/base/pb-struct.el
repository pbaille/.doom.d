;;; pb/xp/structs.el -*- lexical-binding: t; -*-

(require 'pb)
(require 'pb-destructure)
(require 'pb-flow)

(pb/comment
 :extend-destructure-for-simple-struct
 (eval-when-compile

   (defun pb-destructure/simple-struct-impl (head-sym)
     (lambda (args seed)
       (pb-destructure `(cons _ (list ,@args))
                       seed)))

   (defun pb-destructure/add-simple-struct (sym)
     (pb-destructure/extend sym
                            (pb-destructure/simple-struct-impl sym)))
   (pb-destructure/add-simple-struct 'point))

 (pb/let [(point x y) `(point 1 2)]
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
  (pb/let [(person name age) p] name) ; => \"John\""
  (let* ((pred-sym (pb/join-symbol (list name "?"))))
    `(progn
       (defun ,pred-sym (x)
         ,(format "Check if X is a %s struct."
                  (pb/name name))
         (when (listp x)
           (eq ',name (car-safe x))))

       ,@(seq-map-indexed
          (lambda (m i)
            `(defun ,(pb/join-symbol (list name "." m)) (x)
               ,(format "Get the %s field from the %s struct."
                        (pb/name m)
                        (pb/name name))
               (when (,pred-sym x)
                 (nth ,(1+ i) x))))
          members)

       (defun ,name ,members
         ,(format "Create a new %s struct with the given fields."
                  (pb/name name))
         (list ',name ,@members))

       (pb-destructure/extend ',name
                              (lambda (args seed)
                                (pb-destructure (list 'cons
                                                      (list 'eq '_ '',name)
                                                      (cons 'list args))
                                                seed))))))

(defun pb-struct_run-tests ()

  (eval-when-compile
    (require 'pb-destructure)
    (pb-struct person name age))

  (pb-destructure '(person a b c) '(x z y))

  (cl-assert
   (equal
    (pb/if [(person a b c) '(not-person x z y)]
           (list a b c)
           :ok)
    :ok))

  (cl-assert
   (equal
    (pb/let [x (person "Pierre" 43)]
      (list (person? x)
            (person.name x)))
    '(t "Pierre")))

  (cl-assert
   (equal
    (pb/let [(person name age) (person "Pierre" 43)]
      (list name age))
    '("Pierre" 43)))

  (eval-when-compile
    (pb-struct address street city)
    (pb-struct employee name age address))

  (cl-assert
   (equal
    (pb/let [(as emp
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

'(pb-struct_run-tests)

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
  (pb/let [(maybe x) (maybe 42)] x) ; => 42

With validation:
  (pb-struct_deftag numbox #'numberp)
  (numbox 1)         ; => (numbox . 1)
  (numbox \"hello\")  ; => nil (validation failed)"
  (let ((pred-sym (pb/join-symbol (list tag "?"))))
    `(progn
       (defun ,pred-sym (x)
         ,(format "Return X if it is a %s tagged value." (pb/name tag))
         (and (consp x)
              (eq ',tag (car x))
              ,(if content-pred
                   `(funcall ,content-pred (cdr x))
                 t)
              x))

       (defun ,tag (x &rest xs)
         ,(format "Create a new %s tagged value." (pb/name tag))
         (let ((x (if xs (cons x xs) x)))
           ,(if content-pred
                `(when (funcall ,content-pred x)
                   (cons ',tag x))
              `(cons ',tag x))))

       (pb-destructure/extend ',tag
                              (lambda (args seed)
                                (pb-destructure (car args)
                                                (list 'cdr (list ',pred-sym seed))))))))

(defun pb-struct_deftag-tests ()
  (eval-when-compile
    (pb-struct_deftag maybe))

  (maybe 1)
  (maybe 1 2)

  (cl-assert
   (and
    (pb/eq (maybe 42) '(maybe . 42) (cons 'maybe 42))
    (maybe? (maybe 42))
    (equal (maybe (list 1 2))
           (maybe 1 2))
    (null (maybe? '(other . 42)))
    (pb/eq (pb/let [(maybe x) (maybe 42)] x)
           (pb/let [(maybe x) (cons 'maybe 42)] x)
           (pb/if [(maybe x) (maybe 42)]
                  x
                  :not-maybe)
           42)
    (equal (pb/if [(maybe x) '(other . 42)]
                  x
                  :not-maybe)
           :not-maybe)))

  ;; Test tag with numeric validation
  (eval-when-compile
    (pb-struct_deftag numbox #'numberp))

  (cl-assert
   (and
    (pb/eq (numbox 1) (cons 'numbox 1))
    (null (numbox 'io))
    (equal 1 (pb/let [(numbox x) (numbox 1)] x))
    (null (pb/let [(numbox x) (cons 'numbox 'io)] x)))))

'(pb-struct_deftag-tests)

(pb/comment
 (pb-struct_deftag km2)

 (km2? (km2 :a 1 :b 2 :c (km2 :i 4))))

(provide 'pb-struct)
