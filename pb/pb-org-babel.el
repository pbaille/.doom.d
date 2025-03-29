;;; pb-org-babel.el --- org-babel utils -*- lexical-binding: t; -*-

;; Author: Pierre Baille
;; URL: https://github.com/pbaille
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; org babel utils.

;;; Code:

(require 'km)
(require 'pb-flow)
(require 'cl-lib)
(require 'org)
(require 'org-element)

(defun pb-org-babel_strip-lisp-comments (code)
  "Remove comments from a string of Lisp CODE.
; (semi colon) is assumed to be the inline comment character."
  (replace-regexp-in-string "\\(;.*$\\)" "" code nil nil 1))

(defvar pb-org-babel_custom-params
  (km :clojure
      (km
       :pp (km :args (lambda (args)
                       (append '((:results . "raw pp")
                                 (:wrap . "src clojure"))
                               args))))))

(defun pb-org-babel_add-custom-param (name lang spec)
  "Add a custom param named NAME for org LANG block.
SPEC:"
  (cl-assert (and (keywordp name)
                  (keywordp lang)
                  (km? spec)))
  (setq pb-org-babel_custom-params
        (km_put pb-org-babel_custom-params
                (list lang name)
                spec)))

(defun pb-org-babel_execute-src-block-hook (fun &optional arg info params)
  "Wraps org-babel-execute-src-block function."
  (pb_if [(list lang content args) info
          custom-params (km_get pb-org-babel_custom-params (pb_keyword lang))
          ;; because of inline comments bug
          content (if (equal "clojure" lang)
                      (pb-org-babel_strip-lisp-comments content)
                    content)
          (cons content args) (seq-reduce (pb_fn [(cons content args) (cons k wrappers)]
                                                 (if (assoc k args)
                                                     (cons (pb_if [f (km_get wrappers :content)]
                                                                  (funcall f content)
                                                                  content)
                                                           (pb_if [f (km_get wrappers :args)]
                                                                  (funcall f args)
                                                                  args))
                                                   (cons content args)))
                                          (km_entries custom-params)
                                          (cons content args))
          info (pb-> info (sq_put 1 content) (sq_put 2 args))]
         (funcall fun arg info params)
         (funcall fun arg info params)))

(defun pb-org-babel_insert-result-hook (fun result result-params info &rest more)
  "Wraps org-babel-insert-result function."
  (pb_if [(list lang content args) info
          ;; because of inline comment bug
          content (if (equal "clojure" lang)
                      (pb-org-babel_strip-lisp-comments content)
                    content)
          custom-params (km_get pb-org-babel_custom-params (pb_keyword lang))]
         (pb_if [result (seq-reduce (pb_fn [result (cons k wrappers)]
                                           (if (assoc k args)
                                               (pb_if [f (km_get wrappers :result)]
                                                      (funcall f result)
                                                      result)
                                             result))
                                    (km_entries custom-params)
                                    result)]
                (apply fun result
                       result-params
                       info
                       more))
         (apply fun result result-params info more)))

(advice-add 'org-babel-execute-src-block :around #'pb-org-babel_execute-src-block-hook)

(advice-add 'org-babel-insert-result :around #'pb-org-babel_insert-result-hook)

(require 'treesit)

(defvar pb-org-babel_lang->treesit-lang
  '((emacs-lisp . elisp)))

(defun pb-org-babel_setup-treesit-ranges ()
  "Configure treesit parsers for all src blocks in the current org buffer.
Each src block will get a parser corresponding to its language."
  (interactive)
  (when (derived-mode-p 'org-mode)
    (let ((src-blocks '())
          (parsers '()))
      ;; First pass: collect all src blocks and their languages
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "^[ \t]*#\\+begin_src[ \t]+\\([^ \t\n]+\\)" nil t)
          (let* ((lang (match-string-no-properties 1))
                 (lang-symbol (intern lang))
                 (beg (match-end 0))
                 (block-end (save-excursion
                              (when (re-search-forward "^[ \t]*#\\+end_src" nil t)
                                (match-beginning 0)))))
            (when block-end
              (forward-line)
              (setq beg (point))
              (push (list lang-symbol beg block-end) src-blocks)))))

      ;; Second pass: create parsers for each language
      (dolist (lang-blocks (seq-group-by #'car src-blocks))
        (let* ((lang (car lang-blocks))
               (blocks (cdr lang-blocks))
               (ranges (mapcar (lambda (block)
                                 (cons (nth 1 block) (nth 2 block)))
                               blocks))
               (lang (or (alist-get lang pb-org-babel_lang->treesit-lang)
                         lang)))
          (print lang)
          (condition-case nil
              (let ((parser (or (treesit-parser-create lang)
                                (message "Warning: Could not create parser for %s" lang))))
                (print parser)
                (print ranges)
                (when parser
                  (push parser parsers)
                  (treesit-parser-set-included-ranges parser ranges)))
            (error (message "Failed to configure parser for %s" lang)))))

      (message "Configured %d parsers for %d src blocks"
               (length parsers) (length src-blocks))
      parsers)))

(require 'pb-org)

(defun pb-org-babel_add-treesit-range-for-block ()
  "Add a treesit range for the current src block."
  (interactive)
  (when-let ((bounds (pb-org_code-block-content-bounds)))
    (let* ((lang (pb-org_code-block-language))
           (treesit-lang (or (alist-get (intern lang) pb-org-babel_lang->treesit-lang)
                             (intern lang)))
           (parser (condition-case nil
                       (treesit-parser-create treesit-lang)
                     (error (message "Failed to create parser for %s" lang)
                            nil))))
      (when parser
        (treesit-parser-set-included-ranges parser (list bounds))
        (message "Added treesit range for %s block" lang)
        parser))))

(provide 'pb-org-babel)
;;; pb-org-babel.el ends here.
