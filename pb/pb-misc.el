;;; pb-misc.el --- utils -*- lexical-binding: t; -*-

;; Author: Pierre Baille
;; URL: https://github.com/pbaille
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Utils.

;;; Code:

(require 'evil)

(defun pb-misc_open-google ()
  (interactive)
  (xwidget-webkit-browse-url "https://www.google.com/"))

(defun pb-misc_spit (string file)
  "Print STRING into FILE, if files exists, delete it, if not create it."
  (with-temp-buffer
    (insert string)
    (delete-file file)
    (make-directory (file-name-parent-directory file) t)
    (write-region (point-min) (point-max) file t)))

(defun pb-misc_insert-open-paren ()
  (interactive)
  (insert "()")
  (backward-char)
  '(execute-kbd-macro (kbd "(")))

(defun pb-misc_goto-next-opening-delimiter ()
  (interactive)
  (forward-char)
  (re-search-forward "[(\\[\\{]")
  (backward-char))

(defun pb-misc_goto-prev-opening-delimiter ()
  (interactive)
  (re-search-backward "[(\\[\\{]"))

(defun pb-misc_toggle-level-hiding (arg)
  (interactive "p")
  (hs-life-goes-on
   (if (or (> arg 1) (hs-already-hidden-p))
       (progn (hs-show-block) (hs-hide-level arg))
     (hs-hide-block)))
  (backward-char))

(defun pb-misc_toggle-hiding ()
  (interactive)
  (hs-toggle-hiding)
  (backward-char))

(defun pb-misc_kill-all-dired-buffers ()
  "Kill all dired buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (or (eq major-mode 'dired-mode)
                (eq major-mode 'dired-sidebar-mode))
        (kill-buffer buffer)))))

(defun pb-misc_count-indentation (line)
  "Count leading spaces in a LINE."
  (if (string-match "^\\s-+" line)
      (length (match-string 0 line))
    0))

(defun pb-misc_remove-leading-spaces (str n)
  "Remove the first N space characters from the beginning of STR."
  (let ((result str)
        (count n))
    (while (and (> count 0) (string-prefix-p " " result))
      (setq result (substring result 1))
      (setq count (1- count)))
    result))

(defun pb-misc_clear-message-buffer ()
  "Clear the *Messages* buffer."
  (interactive)
  (with-current-buffer "*Messages*"
    (read-only-mode -1)
    (erase-buffer)
    (read-only-mode 1)))

(defun pb-misc_switch-to-message-buffer ()
  (interactive)
  (with-current-buffer "*Messages*"
    (goto-char (point-max)))
  (display-buffer "*Messages*"))

(defun pb-misc_select-vterm-buffer ()
  "Display a list of vterm buffers and return the name of the selected one.
Uses `consult--read` to create an interactive selection menu of all vterm
buffers. Returns the buffer name of the selected vterm buffer."
  (interactive)
  (let* ((repl-buffers (seq-filter
                        (lambda (buffer)
                          (string-match-p "\\*.*vterm.*\\*" (buffer-name buffer)))
                        (buffer-list)))
         (buffer-names (mapcar #'buffer-name repl-buffers))
         (buffer-alist (cl-mapcar #'cons buffer-names repl-buffers)))
    (let ((selected (consult--read
                     buffer-names
                     :prompt "Select vterm buffer: "
                     :require-match t
                     :sort nil)))
      (switch-to-buffer
       (buffer-name (cdr (assoc selected buffer-alist)))))))

(defun pb-misc_window-split (buffer)
  "Split the current window vertically, displaying BUFFER."
  (let* ((below-window (window-in-direction 'below))
         (already-there (when below-window
                          (eq (window-buffer below-window)
                              (get-buffer buffer)))))
    (if (not already-there)
        (let* ((b1 (current-buffer))
               (b2 (or (get-buffer buffer)
                       b1)))
          (switch-to-buffer b2)
          (evil-window-split (* 2 (/ (window-body-height) 3)) nil)
          (switch-to-buffer b1)))))

(progn :scrolling

       (defun scroll-up-with-cursor ()
         "Scroll up one line and keep cursor on the same visual line."
         (interactive)
         (scroll-up-line 1)
         (next-line 1))

       (defun scroll-down-with-cursor ()
         "Scroll down one line and keep cursor on the same visual line."
         (interactive)
         (scroll-down-line 1)
         (previous-line 1)))

(provide 'pb-misc)
;;; pb-misc.el ends here.
