;;; pb-gptel.el --- utils -*- lexical-binding: t; -*-

;; Author: Pierre Baille
;; URL: https://github.com/pbaille
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Utils.

;;; Code:

(require 'pb-misc)

(defun pb-gptel_new-session-above ()
  "Open a new org buffer with gptel-mode activated."
  (interactive)
  (let* ((buffer-name (concat "*" (file-name-base) "_GPT*"))
         (buffer (get-buffer-create (concat "*" (file-name-base) "_GPT*"))))
    (with-current-buffer buffer
      (erase-buffer)
      (org-mode)
      (gptel-mode)
      (insert "*** ")
      (point-max)
      (evil-insert-state))
    (pb-misc_window-split buffer)
    (windmove-down)))

(provide 'pb-gptel)
;;; pb-gptel.el ends here.
