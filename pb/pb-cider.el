;;; pb-cider.el --- Cider utils -*- lexical-binding: t; -*-

;; Author: Pierre Baille
;; URL: https://github.com/pbaille
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1") (cider "1.15.0"))

;;; Commentary:

;; Cider helpers.

;;; Code:

(require 'cider)
(require 'consult)

(defun pb-cider_eval! (code)
  "Eval some CODE using cider.
The `current-buffer' has to be a buffer recognized by cider for this to work,"
  (cider-interactive-eval code
                          nil nil
                          (cider--nrepl-pr-request-map)))

(defun pb-cider_kill-dead-buffers ()
  "Kill all dead cider repl buffers."
  (princ "killing dead cider repl buffers")
  (dolist (buffer (buffer-list))
    (when (and (buffer-live-p buffer)
               (string-match-p "^\\*cider-repl" (buffer-name buffer))
               (not (process-live-p (get-buffer-process buffer))))
      (kill-buffer buffer))))

(defun pb-cider_select-repl-buffer ()
  "List and select a CIDER REPL buffer using consult."
  (interactive)
  (let* ((repl-buffers (seq-filter
                        (lambda (buffer)
                          (string-match-p "\\*cider-repl" (buffer-name buffer)))
                        (buffer-list)))
         (buffer-names (mapcar (lambda (buffer)
                                 (let ((name (buffer-name buffer)))
                                   (replace-regexp-in-string "\\*cider-repl \\([^:]*\\).*\\*" "\\1" name)))
                               repl-buffers))
         (buffer-alist (cl-mapcar #'cons buffer-names repl-buffers)))
    (let ((selected (consult--read
                     buffer-names
                     :prompt "Select CIDER REPL: "
                     :require-match t
                     :sort nil)))
      (buffer-name (cdr (assoc selected buffer-alist))))))

(defun pb-cider_goto-repl ()
  "Let the user choose a repl buffer and switch the buffer to it."
  (interactive)
  (let ((repl-buffer (pb-cider_select-repl-buffer)))
    (when repl-buffer
      (switch-to-buffer repl-buffer))))

(provide 'pb-cider)
;;; pb-cider.el ends here.
