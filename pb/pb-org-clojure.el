;;; pb-org-clojure.el --- org clojure utils -*- lexical-binding: t; -*-

;; Author: Pierre Baille
;; URL: https://github.com/pbaille
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; org clojure utils.

;;; Code:

(require 'cider)
(require 'org-element)
(require 'pb-org-babel)
(require 'pb-cider)
(require 'sorg)
(require 'flycheck)
(require 'symex)

(defun pb-org-clojure_refresh-dynamic-font-lock-keywords (buffer ns)
  "Install font-lock rules according to NS for BUFFER.
The *org-src-fontification:clojure-mode* buffer is used to fontify clojure code.
For blocks to be correctly fontified, we need to install those using cider."
  (with-current-buffer (get-buffer-create
                        buffer)
    (setq-local cider-buffer-ns ns)
    (font-lock-remove-keywords nil cider--dynamic-font-lock-keywords)
    (setq-local cider--dynamic-font-lock-keywords
                (cider--compile-font-lock-keywords
                 (cider-resolve-ns-symbols ns)
                 (cider-resolve-ns-symbols (cider-resolve-core-ns))))
    (font-lock-add-keywords nil cider--dynamic-font-lock-keywords 'end)
    (font-lock-flush)))

(defun pb-org-clojure_get-clojure-namespace ()
  "Retreive the clojure namespace of the current org buffer.
The ns declaration is assumed to be the first clojure block of the file."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "#\\+begin_src clojure" nil t)
      (let* ((element (org-element-context)))
        (when (eq (org-element-type element) 'src-block)
          (let ((block-content (org-element-property :value element)))
            (string-match "(ns \\([^ ]+\\)" block-content)
            (list :ns-name (string-trim-right (match-string 1 block-content))
                  :ns-form block-content)))))))

(defun pb-org-clojure_edit-src-code-hook (fun &optional code buf-name)
  "Code to run when around `org-edit-src-code'.
Threads CODE and BUF-NAME arguments directly to `org-edit-src-code' FUN.
Refresh fontification of the edit src buffer according to the corresponding
org file clojure namespace."
  (let ((clojure-ns (km_get (pb-org-clojure_get-clojure-namespace)
                            :ns-name)))
    (funcall fun code buf-name)
    (when clojure-ns
      (pb-org-clojure_refresh-dynamic-font-lock-keywords
       (buffer-name (current-buffer))
       clojure-ns)
      (flycheck-mode -1)
      (symex-mode-interface))))

(advice-add 'org-edit-src-code :around #'pb-org-clojure_edit-src-code-hook)
(advice-add 'org-edit-src-exit :after #'sorg-enter-mode)

(defun pb-org-clojure_jack-in ()
  "Setup clojure literate org buffer.
- cider-jack-in-clj if necessary,
- send top block ns form to the repl,
- set the corresponding namespace for code blocks fontification."
  (interactive)
  (let ((buffer (current-buffer)))
    (if (not (cider-connected-p))
        (if (yes-or-no-p "Start Cider repl? ")
            (progn
              (message "Starting cider repl")
              (call-interactively #'cider-jack-in-clj)))
      (with-current-buffer buffer
        (pb_let [(km ns-name ns-form) (pb-org-clojure_get-clojure-namespace)]
            (nrepl-request:eval ns-form
                                (lambda (res)
                                  '(print (list "ns form evaluated"
                                                :ns ns-name
                                                :status (nrepl-dict-get res "status")))
                                  (when (equal "state" (car (nrepl-dict-get res "status")))
                                    (pb-org-clojure_refresh-dynamic-font-lock-keywords
                                     " *org-src-fontification:clojure-mode*"
                                     ns-name)
                                    (with-current-buffer buffer (revert-buffer))))
                                (cider-current-repl nil 'ensure)
                                nil nil nil))))))

(provide 'pb-org-clojure)
;;; pb-org-clojure.el ends here
