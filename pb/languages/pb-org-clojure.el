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

(defun pb-org-clojure/refresh-dynamic-font-lock-keywords (buffer ns)
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

(defvar-local pb-org-clojure/default-clojure-namespace
    (list :ns-name "user"
          :ns-form "(ns user)"))

(defun pb-org-clojure/get-clojure-namespace ()
  "Retreive the clojure namespace of the current org buffer.
The ns declaration is assumed to be the first clojure block of the file."
  (when (eq major-mode 'org-mode)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (when (re-search-forward "#\\+begin_src clojure" nil t)
          (let* ((element (org-element-context)))
            (when (eq (org-element-type element) 'src-block)
              (let ((block-content (org-element-property :value element)))
                (string-match "(ns \\([^ ]+\\)" block-content)
                ;; (print block-content)
                (if-let ((matched (match-string 1 block-content)))
                  (list :ns-name (string-trim-right matched)
                        :ns-form block-content)
                  pb-org-clojure/default-clojure-namespace)))))))))

(defun pb-org-clojure/edit-src-code-hook (fun &optional code buf-name)
  "Code to run when around `org-edit-src-code'.
Threads CODE and BUF-NAME arguments directly to `org-edit-src-code' FUN.
Refresh fontification of the edit src buffer according to the corresponding
org file clojure namespace."
  (let ((clojure-ns (km/get (pb-org-clojure/get-clojure-namespace)
                            :ns-name)))
    (funcall fun code buf-name)
    (when clojure-ns
      (pb-org-clojure/refresh-dynamic-font-lock-keywords
       (buffer-name (current-buffer))
       clojure-ns)
      (flycheck-mode -1)
      (symex-mode-interface))
    t))

(advice-add 'org-edit-src-code :around #'pb-org-clojure/edit-src-code-hook)

(advice-add 'org-edit-src-exit :after #'sorg/enter-mode)

(defun pb-org-clojure/set-local-vars ()
  (interactive)
  (pb/let [(km ns-name ) (pb-org-clojure/get-clojure-namespace)]
      (setq-local cider-buffer-ns ns-name)
    (setq-local indent-line-function #'pb-org-clojure/indent-line-function)))

(defun pb-org-clojure/jack-in_BACKUP ()
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
        (pb/let [(km ns-name ns-form) (pb-org-clojure/get-clojure-namespace)]
            (progn (print (list "set local vars"
                                buffer
                                ns-name))
                   (setq-local cider-buffer-ns ns-name)
                   (setq-local indent-line-function #'pb-org-clojure/indent-line-function)
                   (nrepl-request:eval ns-form
                                       (lambda (res)
                                         '(print (list "ns form evaluated"
                                                       :ns ns-name
                                                       :status (nrepl-dict-get res "status")))
                                         (setq-local cider-buffer-ns ns-name)
                                         (when (equal "state" (car (nrepl-dict-get res "status")))
                                           (pb-org-clojure/refresh-dynamic-font-lock-keywords
                                            " *org-src-fontification:clojure-mode*"
                                            ns-name)
                                           (with-current-buffer buffer
                                             (revert-buffer))))
                                       (cider-current-repl nil 'ensure)
                                       nil nil nil)))))))

(defun pb-org-clojure/jack-in ()
  "Setup a Clojure literate Org buffer for REPL interaction.

This function performs the following actions:
1. Checks if a CIDER REPL is connected and offers to start one if not
2. Extracts the namespace from the first Clojure src block in the buffer
3. Sets local variables for proper namespace handling
4. Evaluates the namespace form in the REPL
5. Refreshes syntax highlighting based on the namespace
6. Updates the buffer display appropriately

This allows for proper fontification and evaluation context in Org Clojure blocks."
  (interactive)
  (let ((buffer (current-buffer)))
    (if (not (cider-connected-p))
        (if (yes-or-no-p "Start Cider repl? ")
            (progn
              (message "Starting cider repl")
              (call-interactively #'cider-jack-in-clj)))
      (with-current-buffer buffer
        (pb/let [(km ns-name ns-form) (pb-org-clojure/get-clojure-namespace)]
          (progn (message "Setting local vars for %s with namespace %s"
                          (buffer-name buffer)
                          ns-name)
                 (setq-local cider-buffer-ns ns-name)
                 (setq-local indent-line-function #'pb-org-clojure/indent-line-function)
                 (nrepl-request:eval ns-form
                                     (lambda (res)
                                       (setq-local cider-buffer-ns ns-name)
                                       (when (equal "done" (car (nrepl-dict-get res "status")))
                                         (pb-org-clojure/refresh-dynamic-font-lock-keywords
                                          " *org-src-fontification:clojure-mode*"
                                          ns-name)
                                         (with-current-buffer buffer
                                           ;; Only revert if file exists and is saved
                                           (if (and (buffer-file-name)
                                                    (not (buffer-modified-p)))
                                               (revert-buffer nil t)
                                             ;; Otherwise just refresh font-lock
                                             (font-lock-flush)))))
                                     (cider-current-repl nil 'ensure)
                                     nil nil nil)))))))

(defun pb-org-clojure/indent-line-function ()
  "Default org-indent-line funtion used in source blocks is openning the edit src buffer.
This is not what we want."
  (if (org-in-block-p (list "SRC"))
      (evil-insert-state)
    (funcall #'org-indent-line)))

(provide 'pb-org-clojure)
;;; pb-org-clojure.el ends here
