;;; pb-reapl.el --- utils -*- lexical-binding: t; -*-

;; Author: Pierre Baille
;; URL: https://github.com/pbaille
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Utils.

;;; Code:
(require 'reapl-mode)
(require 'pb-misc)

(defun pb-reapl_repl ()
  (interactive)
  (unless (process-live-p reapl-mode_receive-proc)
    (reapl-mode_connect))
  (pb-misc_window-split (process-buffer reapl-mode_receive-proc)))

(provide 'pb-reapl)
;;; pb-reapl.el ends here.
