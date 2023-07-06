;;; pb/my-ibuffer.el -*- lexical-binding: t; -*-

(require 'ibuffer-sidebar)

(defun my-ibuffer-sidebar-visit-buffer ()
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

(defun my-ibuffer-sidebar-focus ()
  "Switch to ibuffer-sidebar if opened."
  (interactive)
  (let ((window (get-buffer-window "*:buffers:*")))
    (if window
        (select-window window))))

(defun my-ibuffer-sidebar-jump-to-current-buffer ()
  "focus ibuffer-sidebar with cursor on the line corresponding to the current buffer."
  (interactive)
  (let ((current-buffer (current-buffer)))
    (my-ibuffer-sidebar-focus)
    (goto-char (point-min))
    (unless (search-forward (buffer-name current-buffer) nil t)
      (goto-char (point-min)))
    (beginning-of-line-text)
    (recenter)))

(defun my-ibuffer-projectile-hook-fn ()
  (setq ibuffer-filter-groups (ibuffer-projectile-generate-filter-groups)))

(defun my-ibuffer-sidebar-keybinding-fn ()
  (print "sidebar hook"))

(add-hook 'ibuffer-mode-hook #'my-ibuffer-projectile-hook-fn)

;; trying to bind click1 to visit buffer for sidebar...
'(:map ibuffer-mode-filter-group-map
        :n "<mouse-1>" #'my-ibuffer-sidebar-visit-buffer)
;; should not be obliged to advice...
(advice-add #'ibuffer-mouse-toggle-mark :override #'my-ibuffer-sidebar-visit-buffer)

(add-hook 'ibuffer-sidebar-mode-hook #'my-ibuffer-sidebar-keybinding-fn)

(provide 'my-ibuffer)
