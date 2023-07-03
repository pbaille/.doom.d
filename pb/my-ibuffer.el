;;; pb/my-ibuffer.el -*- lexical-binding: t; -*-

(defun my-ibuffer-sidebar-visit-buffer ()
  "Visit the buffer on this line.
If optional argument SINGLE is non-nil, then also ensure there is only
one window."
  (interactive)
  (let ((buf (ibuffer-current-buffer t)))
    (windmove-right)
    (switch-to-buffer buf)))

(provide 'my-ibuffer)
