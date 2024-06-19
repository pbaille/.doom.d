;;; pb.el --- utils -*- lexical-binding: t; -*-

;; Author: Pierre Baille
;; URL: https://github.com/pbaille
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Utils.

;;; Code:

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
  "Convert the symbol SYM to a keyword."
  (if (keywordp kw)
      (substring (symbol-name kw) 1)))


(defun pb_keyword-to-symbol (kw)
  "Convert the symbol SYM to a keyword."
  (if-let ((name (keyword-name kw)))
      (intern name)
    nil))

(defun pb_test-unprefixed ()
  "Run some assertions about this file"
  (cl-assert
   (and (equal "pierre" (keyword-name :pierre))
        (equal 'pierre (keyword-to-symbol :pierre))
        (equal :pierre (symbol-to-keyword 'pierre)))))

(pb_test)

(provide 'pb)
;;; pb.el ends here.
