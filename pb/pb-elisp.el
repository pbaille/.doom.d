;;; pb-elisp.el --- utils -*- lexical-binding: t; -*-

;; Author: Pierre Baille
;; URL: https://github.com/pbaille
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Utils.

;;; Code:

(require 'symex)
(require 'km)
(require 'pb)
(require 'pb-destructure)
(require 'pb-symex)
(require 'flycheck)
(require 'ielm)

(defvar pb-elisp_result-buffer-name "ELisp_eval")
(defvar pb-elisp_package-separator-char "/")

(defun pb-elisp_send-expression-to-ielm ()
  "Evaluate the expression at point and print the result in the IELM buffer."
  (interactive)
  (if (get-buffer "*ielm*")
      (let* ((expr (or (thing-at-point 'symbol t)
                       (thing-at-point 'sexp t))))
        (with-current-buffer "*ielm*"
          (goto-char (point-max))
          (insert expr)
          (ielm-send-input)))))

(defun pb-elisp_insert-package-prefix ()
  "Insert the package prefix at point based on the current file name."
  (interactive)
  (let* ((file-name (buffer-file-name))
         (base-name (file-name-base file-name))
         ;; Extract the package name (you may need to adjust this based on your file naming convention)
         (package-prefix (concat base-name pb-elisp_package-separator-char)))
    (insert package-prefix)))

(defun pb-elisp_display-expression (expression &optional pp-fn)
  "Display EXPRESSION in a dedicated buffer with proper formatting.
This function creates or switches to a buffer named by
`pb-elisp_result-buffer-name`, clears its content, inserts the
pretty-printed EXPRESSION, sets it to `emacs-lisp-mode', and disables
flycheck-mode."
  (with-current-buffer (get-buffer-create pb-elisp_result-buffer-name)
    (erase-buffer)
    (insert (funcall (or pp-fn #'pp) expression))
    (emacs-lisp-mode)
    (flycheck-mode -1))
  (let ((result-buffer (get-buffer pb-elisp_result-buffer-name)))
    (unless (get-buffer-window result-buffer)
      (display-buffer result-buffer))))

(defun pb-elisp_eval-pretty ()
  "Evaluate and pretty-print the current Lisp expression.
Displays the result in a buffer named 'ELisp_eval'."
  (interactive)
  ;; symex is moving cursor ar the end of current symex before calling this
  (backward-sexp)
  ;; (pb-symex_select-current)
  (pb-elisp_display-expression
   (eval (read (pb-symex_current-as-string)))
   #'km_pp))

(require 'treesit)

(defun pb-elisp_treesit-parser-setup ()
  "Setup tree-sitter parser for current elisp buffer."
  (when (treesit-language-available-p 'elisp)
    (treesit-parser-create 'elisp)))

(add-hook 'emacs-lisp-mode-hook
          #'pb-elisp_treesit-parser-setup)

(provide 'pb-elisp)
;;; pb-elisp.el ends here.
