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
  "List and select a CIDER REPL buffer using consult.
Uses consult--read to create an interactive selection menu of all CIDER REPL
buffers with live previewing."
  (interactive)
  (let* ((repl-buffers (seq-filter
                        (lambda (buffer)
                          (and (buffer-live-p buffer)
                               (with-current-buffer buffer
                                 (derived-mode-p 'cider-repl-mode))))
                        (buffer-list)))
         (buffer-names (mapcar #'buffer-name repl-buffers))
         (selected (consult--read
                    buffer-names
                    :prompt "Select CIDER REPL: "
                    :sort nil
                    :require-match t
                    :category 'buffer
                    :state (consult--buffer-state))))
    (when selected
      selected)))

(defun pb-cider_goto-repl ()
  "Let the user choose a repl buffer and switch the buffer to it."
  (interactive)
  (let ((repl-buffer (pb-cider_select-repl-buffer)))
    (when repl-buffer
      (switch-to-buffer repl-buffer))))

(provide 'pb-cider)
;;; pb-cider.el ends here.
