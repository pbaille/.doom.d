;;; pb-dired.el --- utils -*- lexical-binding: t; -*-

;; Author: Pierre Baille
;; URL: https://github.com/pbaille
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Utils.

;;; Code:

(require 'evil)
(require 'dired)
(require 'dired-subtree)

(defun pb-dired/sidebar-dwim ()
  "Visit the buffer on this line.
   If optional argument SINGLE is non-nil, then also ensure there is only
   one window."
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (when file
      (if (file-directory-p file)
          (dired-subtree-toggle)
        (let ((buffer (or (find-buffer-visiting file)
                          (find-file-noselect file))))
          (when buffer
            (windmove-right)
            (switch-to-buffer buffer)
            (dired-sidebar-toggle-sidebar)
            (tree-browser/navigate-buffer)))))))

(defun pb-dired/show-file ()
  "Show the file at point in the adjacent window (right) without leaving the sidebar"
  (interactive)
  (when-let ((file (dired-get-file-for-visit)))
    (save-selected-window
      (windmove-right)
      (if (file-directory-p file)
          (dired file)
        (switch-to-buffer (or (find-buffer-visiting file)
                              (find-file-noselect file)))))))

(defun pb-dired/sidebar-mouse-dwim (event)
  "Perform an action on mouse click in dired-sidebar."
  (interactive "e")
  ;; Perform your desired action here, such as opening the file or executing a command.
  ;; For example, you can use `(dired-sidebar-goto-file-other-window)` to open the file in another window.

  ;; Preserve the cursor position
  (mouse-set-point event)
  (print "pb-dired/mouse")
  (let ((file (dired-get-file-for-visit)))
    (when file
      (if (file-directory-p file)
          (dired-subtree-toggle)
        (pb-dired/show-file)))))

(defun pb-dired/sidebar-close-all ()
  "Close all dired-sidebar buffers before exiting Emacs."
  (interactive)
  (walk-windows
   (lambda (win)
     (when (and (window-dedicated-p win)
                (string-match-p "^:~/" (buffer-name (window-buffer win))))
       (delete-window win))))
  (dolist (buf (buffer-list))
    (when (string-match-p "^:~/" (buffer-name buf))
      (kill-buffer buf))))

(advice-add #'doom/quicksave-session :before #'pb-dired/sidebar-close-all)
(add-hook 'kill-emacs-hook #'pb-dired/sidebar-close-all)

(defun pb-dired/sidebar-reset ()
  (interactive)
  (pb-misc/kill-all-dired-buffers)
  (dired-sidebar-toggle-sidebar))

(advice-add #'dired-sidebar-mouse-subtree-cycle-or-find-file :override #'pb-dired/sidebar-mouse-dwim)

(provide 'pb-dired)
;;; pb-dired.el ends here.
