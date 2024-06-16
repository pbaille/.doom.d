;;; pb-sidebars.el --- utils -*- lexical-binding: t; -*-

;; Author: Pierre Baille
;; URL: https://github.com/pbaille
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; pb-sidebars utility.

;;; Code:

(require 'dired-sidebar)
(require 'ibuffer-sidebar)

(defun pb-sidebars_toggle ()
  (interactive)
  (let ((dired-sidebar-visible (dired-sidebar-showing-sidebar-p))
        (ibuffer-sidebar-visible (ibuffer-sidebar-showing-sidebar-p)))
    (if (or dired-sidebar-visible ibuffer-sidebar-visible)
        (progn (if dired-sidebar-visible (dired-sidebar-hide-sidebar))
               (if ibuffer-sidebar-visible (ibuffer-sidebar-hide-sidebar)))
      (progn (if (not dired-sidebar-visible)
                 (dired-sidebar-show-sidebar))
             (if (not ibuffer-sidebar-visible)
                 (ibuffer-sidebar-show-sidebar))))))

(provide 'pb-sidebars)
;;; pb-sidebars.el ends here.
