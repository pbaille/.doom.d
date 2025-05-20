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

(defun pb/first (s)
  "Return the first element of the sequence S."
  (car s))

(defun pb/second (s)
  "Return the second element of the sequence S."
  (cadr s))

(defun pb/third (s)
  "Return the third element of the sequence S."
  (caddr s))

(defun pb/symbol-to-keyword (sym)
  "Convert the symbol SYM to a keyword."
  (intern (concat ":" (symbol-name sym))))


(defun pb/keyword-name (kw)
  "Get the name of KW (its `symbol-name' without colon)."
  (if (keywordp kw)
      (substring (symbol-name kw) 1)))


(defun pb/keyword-to-symbol (kw)
  "Convert the keyword KW to a symbol."
  (if-let ((name (pb/keyword-name kw)))
      (intern name)
    nil))

(defun pb/name (x)
  "Return the name (string) of X (string, keyword or symbol)."
  (cond ((stringp x) x)
        ((keywordp x) (pb/keyword-name x))
        ((symbolp x) (symbol-name x))))

(defun pb/join-string (xs &optional sep)
  "Build a string from given XS, using SEP as join."
  (mapconcat #'pb/name xs (or sep "")))

(defun pb/join-symbol (xs &optional sep)
  "Build a symbol from given XS, using SEP as join."
  (intern (pb/join-string xs sep)))

(defun pb/join-keyword (xs &optional sep)
  "Build a keyword from given XS, using SEP as join."
  (intern (concat ":" (pb/join-string xs sep))))

(defun pb/string (&rest xs)
  "Build a string from the names of given XS (strings, keywords or symbols)."
  (mapconcat #'pb/name xs ""))

(defun pb/symbol (&rest xs)
  "Build a symbol joining the names of given XS with '-'."
  (pb/join-symbol xs "-"))

(defun pb/keyword (&rest xs)
  "Build a keyword joining the names of given XS with '-'."
  (pb/join-keyword xs "-"))

(defmacro pb-> (x &rest forms)
  "Threads X through FORMS as first argument."
  (seq-reduce
   (lambda (result form)
     (if (seqp form)
         `(,(car form) ,result ,@(cdr form))
       (list form result)))
   forms
   x))

(defmacro pb->> (x &rest forms)
  "Threads X through FORMS as last argument."
  (seq-reduce
   (lambda (result form)
     (if (seqp form)
         `(,@form ,result)
       (list form result)))
   forms
   x))

(defmacro pb->/ (&rest forms)
  "Thread the first argument into following FORMS.
   using the _ placeholder to determine threaded value positioning."
  (cl-destructuring-bind (ret . bindings) (reverse forms)
    `(let* ,(seq-reduce
             (lambda (bindings form)
               (cons (list '_ form) bindings))
             bindings
             (list))
       ,ret)))

(defmacro pb/comment (&rest _)
  "A macro that always expands to nil."
  ())

(defun pb/slurp (file-path)
  "Return the contents of FILE-PATH as a string.
   Returns nil if the file doesn't exist or can't be read."
  (condition-case nil
      (with-temp-buffer
        (insert-file-contents file-path)
        (buffer-string))
    (file-error nil)))

(defmacro pb/setq (var expr)
  `(setq ,var (let ((_ ,var)) ,expr)))

(defun pb/eq (&rest xs)
  "Compares every element of XS for equality using `equal'."
  (if xs
      (let ((first (car xs)))
        (cl-every (lambda (x) (equal first x))
                  (cdr xs)))
    t))

(defun pb/hash (x)
  (substring (md5 (format "%s" x)) 0 8))

(defun pb/test ()
  "Run some assertions about this file."
  (cl-assert
   (and (equal "pierre" (pb/keyword-name :pierre))
        (equal 'pierre (pb/keyword-to-symbol :pierre))
        (equal :pierre (pb/symbol-to-keyword 'pierre))
        (equal :foo-bar (pb/keyword "foo" "bar"))
        (equal 'foo-bar (pb/symbol "foo" "bar"))
        (equal 'foo-bar (pb/symbol "foo" 'bar))
        (equal :foo_bar (pb/join-keyword (list "foo" "bar") "_"))
        (equal 'foo_bar (pb/join-symbol (list "foo" "bar") "_"))
        (not (pb/comment non sense))
        (stringp (pb/slurp "~/.doom.d/pb/base/pb.el"))))

  (progn (defvar pb/swap-testvar 0)
         (setq pb/swap-testvar 0)
         (pb/setq pb/swap-testvar (1+ _))
         (cl-assert (equal pb/swap-testvar 1)))

  (cl-assert
   (and (pb/eq 1 1)
        (pb/eq "hello" "hello")
        (pb/eq '(1 2 3) '(1 2 3))
        (pb/eq :keyword :keyword)
        (pb/eq 'symbol 'symbol)
        (not (pb/eq 1 2))
        (not (pb/eq "hello" "world"))
        (not (pb/eq '(1 2 3) '(1 2)))
        (not (pb/eq :keyword :other))
        (pb/eq) ; empty call returns t
        (pb/eq 42) ; single element returns t
        (pb/eq 42 42 42) ; multiple equal elements
        (not (pb/eq 42 42 43)))))

(pb/test)

(provide 'pb)
;;; pb.el ends here.
