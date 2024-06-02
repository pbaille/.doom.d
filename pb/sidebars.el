;;; pb/sidebars.el -*- lexical-binding: t; -*-

(require 'dired-sidebar)
(require 'ibuffer-sidebar)

(defun pb/toggle-sidebars () (interactive)
       (let ((dired-sidebar-visible (dired-sidebar-showing-sidebar-p))
             (ibuffer-sidebar-visible (ibuffer-sidebar-showing-sidebar-p)))
         (if (or dired-sidebar-visible ibuffer-sidebar-visible)
             (progn (if dired-sidebar-visible (dired-sidebar-hide-sidebar))
                    (if ibuffer-sidebar-visible (ibuffer-sidebar-hide-sidebar)))
           (progn (if (not dired-sidebar-visible)
                      (dired-sidebar-show-sidebar))
                  (if (not ibuffer-sidebar-visible)
                      (ibuffer-sidebar-show-sidebar))))))

(provide 'sidebars)
