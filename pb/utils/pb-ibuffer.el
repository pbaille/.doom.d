;;; pb-ibuffer.el --- ibuffer utils -*- lexical-binding: t; -*-

;; Author: Pierre Baille
;; URL: https://github.com/pbaille
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1") (ibuffer-sidebar))

;;; Commentary:

;; ibuffer helpers.

;;; Code:

(require 'ibuffer-sidebar)

(defun pb-ibuffer_sidebar-visit-buffer ()
  "Visit the buffer associated with the currently selected line in Ibuffer sidebar.

If the buffer is displayed in an existing window, the function selects
that window. Otherwise, it creates a new window to the right and
switches to the buffer.

This function is meant to be used in combination with Ibuffer sidebar
to quickly switch to buffers."
  (interactive)
  (let* ((buf (ibuffer-current-buffer t))
        (window (get-buffer-window buf)))
    (if window
        (select-window window)
        (progn (windmove-right)
               (switch-to-buffer buf)))))

(defun pb-ibuffer_sidebar-focus ()
  "Switch to ibuffer-sidebar if opened."
  (interactive)
  (let ((window (get-buffer-window "*:buffers:*")))
    (if window
        (select-window window))))

(defun pb-ibuffer_sidebar-jump-to-current-buffer ()
  "Focus ibuffer-sidebar on the line corresponding to the current buffer."
  (interactive)
  (let ((current-buffer (current-buffer)))
    (pb-ibuffer_sidebar-focus)
    (goto-char (point-min))
    (unless (search-forward (buffer-name current-buffer) nil t)
      (goto-char (point-min)))
    (beginning-of-line-text)
    (recenter)))

(defun pb-ibuffer_projectile-hook-fn ()
  "Projectile ibuffer hook."
  (setq ibuffer-filter-groups (ibuffer-projectile-generate-filter-groups)))

(add-hook 'ibuffer-mode-hook #'pb-ibuffer_projectile-hook-fn)

;; trying to bind click1 to visit buffer for sidebar...
'(:map ibuffer-mode-filter-group-map
        :n "<mouse-1>" #'pb-ibuffer_sidebar-visit-buffer)
;; should not be obliged to advice...
(advice-add #'ibuffer-mouse-toggle-mark :override #'pb-ibuffer_sidebar-visit-buffer)

(provide 'pb-ibuffer)

;;; pb-ibuffer.el ends here
