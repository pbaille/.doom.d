;;; pb-cider.el --- Cider utils -*- lexical-binding: t; -*-

;; Author: Pierre Baille
;; URL: https://github.com/pbaille
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1") (cider "1.15.0"))

;;; Commentary:

;; Cider helpers.

;;; Code:

(require 'cider)

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

(provide 'pb-cider)
;;; pb-cider.el ends here.
