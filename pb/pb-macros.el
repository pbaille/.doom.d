;;; pb-macros.el -*- lexical-binding: t; -*-

(require 'seq)
(require 'cl-lib)

(defmacro fn (args &rest body)
  "Like lambda but with a shorter name and destructuring."
  (let ((bindings (cl-loop for i from 0 to (- (length args) 1)
                           collect (cons (gensym (format "arg_%s" i)) (nth i args)))))
    `(lambda ,(mapcar #'car bindings)
       ,(seq-reduce (lambda (ret x)
                      `(cl-destructuring-bind ,(cdr x) ,(car x)
                         ,ret))
                    (reverse bindings)
                    (cons 'progn body)))))

(defmacro -> (x &rest forms)
  (seq-reduce
   (lambda (result form)
     (if (seqp form)
         `(,(car form) ,result ,@(cdr form))
       (list form result)))
   forms
   x))

(defmacro dlet (bindings &rest body)
  (seq-reduce
   (lambda (ret binding)
     (cl-destructuring-bind (pat _) binding
       (if (symbolp pat)
           `(let (,binding) ,ret)
         `(cl-destructuring-bind ,@binding ,ret))))
   (reverse (seq-partition bindings 2))
   (cons 'progn body)))

(defmacro >_ (&rest forms)
  "Thread the first argument into following FORMS.
using the _ placeholder to determine threaded value positioning."
  (cl-destructuring-bind (ret . bindings) (reverse forms)
    `(let* ,(seq-reduce
             (lambda (bindings form)
               (cons (list '_ form) bindings))
             bindings
             (list))
       ,ret)))

(defmacro f_ (&rest body)
  `(lambda (_)
     ,@body))

(provide 'pb-macros)
