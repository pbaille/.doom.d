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

(defvar pb-elisp/result-buffer-name "ELisp_eval")
(defvar pb-elisp/package-separator-char "/")

(defun pb-elisp/send-expression-to-ielm ()
  "Evaluate the expression at point and print the result in the IELM buffer."
  (interactive)
  (if (get-buffer "*ielm*")
      (let* ((expr (or (thing-at-point 'symbol t)
                       (thing-at-point 'sexp t))))
        (with-current-buffer "*ielm*"
          (goto-char (point-max))
          (insert expr)
          (ielm-send-input)))))

(defun pb-elisp/insert-package-prefix ()
  "Insert the package prefix at point based on the current file name."
  (interactive)
  (let* ((file-name (buffer-file-name))
         (base-name (file-name-base file-name))
         ;; Extract the package name (you may need to adjust this based on your file naming convention)
         (package-prefix (concat base-name pb-elisp/package-separator-char)))
    (insert package-prefix)))

(defun pb-elisp/display-expression (expression &optional pp-fn)
  "Display EXPRESSION in a dedicated buffer with proper formatting.
   This function creates or switches to a buffer named by
   `pb-elisp/result-buffer-name`, clears its content, inserts the
   pretty-printed EXPRESSION, sets it to `emacs-lisp-mode', and disables
   flycheck-mode."
  (with-current-buffer (get-buffer-create pb-elisp/result-buffer-name)
    (erase-buffer)
    (insert (funcall (or pp-fn #'pp) expression))
    (emacs-lisp-mode)
    (flycheck-mode -1))
  (let ((result-buffer (get-buffer pb-elisp/result-buffer-name)))
    (unless (get-buffer-window result-buffer)
      (display-buffer result-buffer))))

(defun pb-elisp/eval-pretty ()
  "Evaluate and pretty-print the current Lisp expression.
   Displays the result in a buffer named 'ELisp_eval'."
  (interactive)
  ;; symex is moving cursor ar the end of current symex before calling this
  (backward-sexp)
  ;; (pb-symex/select-current)
  (pb-elisp/display-expression
   (eval (read (pb-symex/current-as-string)))
   #'km/pp))

(require 'treesit)

(defun pb-elisp/treesit-parser-setup ()
  "Setup tree-sitter parser for current elisp buffer."
  (when (treesit-language-available-p 'elisp)
    (treesit-parser-create 'elisp)))

(add-hook 'emacs-lisp-mode-hook
          #'pb-elisp/treesit-parser-setup)

(defface font-lock-function-definition-face
  '((t :inherit font-lock-function-name-face))
  "Face used for function names in definition forms like defun, defmacro, etc."
  :group 'font-lock-faces)

(font-lock-add-keywords
 'emacs-lisp-mode
 '(("(\\(def\\(?:un\\|macro\\|subst\\|advice\\|ine-\\w+\\)\\)\\_>[ \t']*\\(\\(?:\\sw\\|\\s_\\)+\\)"
    (2 'font-lock-function-definition-face))
   ("(\\(define-derived-mode\\)\\_>[ \t']*\\(\\(?:\\sw\\|\\s_\\)+\\)"
    (2 'font-lock-variable-name-face))))

(progn :package-prefix

       (defface pb-elisp/namespace-prefix-face
         `((t
            :foreground ,(pb-color (doom-color 'red) (desaturate .8) (darken 0))
            :weight normal))
         "Face for namespace prefixes in elisp (text before a slash in a symbol).")

       (defun pb-elisp/prefix-matcher (limit)
         "Match symbols like 'prefix/name' up to LIMIT, capturing prefix part."
         (when (re-search-forward "\\<\\([a-zA-Z][a-zA-Z0-9-]*\\)\\(/\\)" limit t)
           ;; We found a match - the \\< ensures we match at word boundaries
           ;; We've added a second capture group for the slash
           (let ((prefix-beginning (match-beginning 1))
                 (prefix-end (match-end 1))
                 (slash-beginning (match-beginning 2))
                 (slash-end (match-end 2)))
             ;; Set match data to highlight both the prefix and the slash
             ;; The full match will be from prefix beginning to slash end
             (set-match-data (list prefix-beginning slash-end  ;; full match
                                   prefix-beginning prefix-end  ;; group 1 (prefix)
                                   slash-beginning slash-end))  ;; group 2 (slash)
             t)))

       (font-lock-add-keywords
        'emacs-lisp-mode
        '((pb-elisp/prefix-matcher 1 'pb-elisp/namespace-prefix-face prepend)
          (pb-elisp/prefix-matcher 2 'default prepend))
        1)

       (pb/comment
        (font-lock-remove-keywords
         'emacs-lisp-mode
         '((pb-elisp/prefix-matcher 0 'pb-elisp/namespace-prefix-face prepend)
           (pb-elisp/prefix-matcher 0 'pb-elisp/namespace-prefix-face append)
           (pb-elisp/prefix-matcher 0 'pb-elisp/namespace-prefix-face keep)
           (pb-elisp/prefix-matcher 0 'pb-elisp/namespace-prefix-face)))

        (font-lock-flush))

       font-lock-keywords-alist)

(pb/comment
 (dolist (buffer (buffer-list))
   (with-current-buffer buffer
     (when (derived-mode-p 'emacs-lisp-mode)
       (font-lock-flush)))))

(provide 'pb-elisp)
;;; pb-elisp.el ends here.
