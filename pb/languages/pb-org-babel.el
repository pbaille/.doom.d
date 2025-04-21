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

(defun pb-org-babel/strip-lisp-comments (code)
  "Remove comments from a string of Lisp CODE.
; (semi colon) is assumed to be the inline comment character."
  (replace-regexp-in-string "\\(;.*$\\)" "" code nil nil 1))

(defvar pb-org-babel/custom-params
  (km :clojure
      (km
       :pp (km :args (lambda (args)
                       (append '((:results . "raw pp")
                                 (:wrap . "src clojure"))
                               args))))))

(defun pb-org-babel/add-custom-param (name lang spec)
  "Add a custom param named NAME for org LANG block.
SPEC:"
  (cl-assert (and (keywordp name)
                  (keywordp lang)
                  (km? spec)))
  (setq pb-org-babel/custom-params
        (km/put pb-org-babel/custom-params
                (list lang name)
                spec)))

(defun pb-org-babel/execute-src-block-hook (fun &optional arg info params)
  "Wraps org-babel-execute-src-block function."
  (pb/if [(list lang content args) info
          custom-params (km/get pb-org-babel/custom-params (pb/keyword lang))
          ;; because of inline comments bug
          content (if (equal "clojure" lang)
                      (pb-org-babel/strip-lisp-comments content)
                    content)
          (cons content args) (seq-reduce (pb/fn [(cons content args) (cons k wrappers)]
                                                 (if (assoc k args)
                                                     (cons (pb/if [f (km/get wrappers :content)]
                                                                  (funcall f content)
                                                                  content)
                                                           (pb/if [f (km/get wrappers :args)]
                                                                  (funcall f args)
                                                                  args))
                                                   (cons content args)))
                                          (km/entries custom-params)
                                          (cons content args))
          info (pb-> info (sq/put 1 content) (sq/put 2 args))]
         (funcall fun arg info params)
         (funcall fun arg info params)))

(defun pb-org-babel/insert-result-hook (fun result result-params info &rest more)
  "Wraps org-babel-insert-result function."
  (pb/if [(list lang content args) info
          ;; because of inline comment bug
          content (if (equal "clojure" lang)
                      (pb-org-babel/strip-lisp-comments content)
                    content)
          custom-params (km/get pb-org-babel/custom-params (pb/keyword lang))]
         (pb/if [result (seq-reduce (pb/fn [result (cons k wrappers)]
                                           (if (assoc k args)
                                               (pb/if [f (km/get wrappers :result)]
                                                      (funcall f result)
                                                      result)
                                             result))
                                    (km/entries custom-params)
                                    result)]
                (apply fun result
                       result-params
                       info
                       more))
         (apply fun result result-params info more)))

(advice-add 'org-babel-execute-src-block :around #'pb-org-babel/execute-src-block-hook)

(advice-add 'org-babel-insert-result :around #'pb-org-babel/insert-result-hook)

(progn :treesit
       (require 'treesit)
       (require 'pb-org)

       (defvar pb-org-babel/lang->treesit-lang
         '((emacs-lisp . elisp)
           (clojure . clojure)
           (clojurescript . clojure)
           (clojurec . clojure)))

       (defun pb-org-babel/lang-string->treesit-lang (lang)
           (or (alist-get (intern lang) pb-org-babel/lang->treesit-lang)
               (intern lang)))

       (alist-get (intern "emacs-lisp")
                  pb-org-babel/lang->treesit-lang)

       (progn :prepare-all-sub-ranges

              (defun pb-org-babel/get-src-blocks ()
                (interactive)
                (when (derived-mode-p 'org-mode)
                  (let ((src-blocks '()))
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
                            (push (cons lang-symbol (cons beg block-end)) src-blocks)))))
                    src-blocks)))

              (defun pb-org-babel/get-lang-ranges ()
                (mapcar (pb/fn ((cons lang ranges))
                               (cons (or (alist-get lang pb-org-babel/lang->treesit-lang)
                                         lang)
                                     (seq-reverse (mapcar #'cdr ranges))))
                        (sq/group-by #'car
                                     (pb-org-babel/get-src-blocks))))

              (defun pb-org-babel/setup-language-at-point-function ()
                (setq-local treesit-language-at-point-function
                            (lambda (&rest _)
                              (if (eq major-mode 'org-mode)
                                  (if-let ((lang-str
                                            (and (not (pb-org/at-code-block-p))
                                                 (pb-org/code-block-language))))
                                      (pb-org-babel/lang-string->treesit-lang lang-str)
                                    'org)
                                (treesit-parser-language (car (treesit-parser-list)))))))

              (defun pb-org-babel/init-buffer ()
                (interactive)
                (treesit-parser-create 'org)
                (pb-org-babel/setup-language-at-point-function)
                (let (parsers)
                  (dolist (x (pb-org-babel/get-lang-ranges) parsers)
                    (pb/let [(cons lang ranges) x
                             parser (treesit-parser-create lang)]
                      (treesit-parser-set-included-ranges parser ranges)
                      (push parser parsers)))))

              (defun pb-org-babel/focus-code-block ()
                (interactive)
                (treesit-parser-create 'org)
                (pb-org-babel/setup-language-at-point-function)
                (print "focus")
                (print (save-excursion (pb-org/code-block-goto-beg)
                                       (pb-org/code-block-content-bounds)))
                (if-let ((bounds (save-excursion (pb-org/code-block-goto-beg)
                                                 (pb-org/code-block-content-bounds))))
                    (let* ((lang (pb-org/code-block-language))
                           (treesit-lang (pb-org-babel/lang-string->treesit-lang lang))
                           (parser (condition-case nil
                                       (treesit-parser-create treesit-lang)
                                     (error (message "Failed to create parser for %s" lang)
                                            nil))))
                      (when parser
                        (treesit-parser-set-included-ranges parser (list bounds))
                        (message "bounds: %s" bounds)
                        (message "Added treesit range for %s block" lang)
                        parser)
                      ;; (treesit-parser-set-included-ranges parser (list bounds))
                      )))))

(provide 'pb-org-babel)
;;; pb-org-babel.el ends here.
