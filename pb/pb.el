;;; pb.el --- utils -*- lexical-binding: t; -*-

;; Author: Pierre Baille
;; URL: https://github.com/pbaille
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Utils.

;;; Code:

(require 'cl-lib)
(require 'seq)

(defun pb_first (s)
  "Return the first element of the sequence S."
  (car s))

(defun pb_second (s)
  "Return the second element of the sequence S."
  (cadr s))

(defun pb_third (s)
  "Return the third element of the sequence S."
  (caddr s))

(defun pb_symbol-to-keyword (sym)
  "Convert the symbol SYM to a keyword."
  (intern (concat ":" (symbol-name sym))))


(defun pb_keyword-name (kw)
  "Get the name of KW (its `symbol-name' without colon)."
  (if (keywordp kw)
      (substring (symbol-name kw) 1)))


(defun pb_keyword-to-symbol (kw)
  "Convert the keyword KW to a symbol."
  (if-let ((name (pb_keyword-name kw)))
      (intern name)
    nil))

(defun pb_name (x)
  "Return the name (string) of X (string, keyword or symbol)."
  (cond ((stringp x) x)
        ((keywordp x) (pb_keyword-name x))
        ((symbolp x) (symbol-name x))))

(defun pb_join-string (xs &optional sep)
  "Build a string from given XS, using SEP as join."
  (mapconcat #'pb_name xs (or sep "")))

(defun pb_join-symbol (xs &optional sep)
  "Build a symbol from given XS, using SEP as join."
  (intern (pb_join-string xs sep)))

(defun pb_join-keyword (xs &optional sep)
  "Build a keyword from given XS, using SEP as join."
  (intern (concat ":" (pb_join-string xs sep))))

(defun pb_string (&rest xs)
  "Build a string from the names of given XS (strings, keywords or symbols)."
  (mapconcat #'pb_name xs ""))

(defun pb_symbol (&rest xs)
  "Build a symbol joining the names of given XS with '-'."
  (pb_join-symbol xs "-"))

(defun pb_keyword (&rest xs)
  "Build a keyword joining the names of given XS with '-'."
  (pb_join-keyword xs "-"))

(defmacro pb-> (x &rest forms)
  "Threads X through FORMS as first argument."
  (seq-reduce
   (lambda (result form)
     (if (seqp form)
         `(,(car form) ,result ,@(cdr form))
       (list form result)))
   forms
   x))

(defmacro pb->_ (&rest forms)
  "Thread the first argument into following FORMS.
using the _ placeholder to determine threaded value positioning."
  (cl-destructuring-bind (ret . bindings) (reverse forms)
    `(let* ,(seq-reduce
             (lambda (bindings form)
               (cons (list '_ form) bindings))
             bindings
             (list))
       ,ret)))

(defun pb_test ()
  "Run some assertions about this file."
  (cl-assert
   (and (equal "pierre" (pb_keyword-name :pierre))
        (equal 'pierre (pb_keyword-to-symbol :pierre))
        (equal :pierre (pb_symbol-to-keyword 'pierre))
        (equal :foo-bar (pb_keyword "foo" "bar"))
        (equal 'foo-bar (pb_symbol "foo" "bar"))
        (equal :foo_bar (pb_join-keyword (list "foo" "bar") "_"))
        (equal 'foo_bar (pb_join-symbol (list "foo" "bar") "_")))))

(pb_test)

(provide 'pb)

(require 'pb-destructure)
(defalias 'pb_let 'pb-destructure_let)
(defalias 'pb_fn 'pb-destructure_fn)
(defalias 'pb_defun 'pb-destructure_defun)

(require 'pb-flow)
(defalias 'pb_if 'pb-flow)
(defalias 'pb_fm 'pb-flow_fn)


;;; pb.el ends here.
