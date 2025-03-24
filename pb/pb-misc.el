;;; pb-misc.el --- utils -*- lexical-binding: t; -*-

;; Author: Pierre Baille
;; URL: https://github.com/pbaille
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Utils.

;;; Code:

(require 'evil)
(require 'consult)
(require 'hideshow)

(defun pb-misc_open-google ()
  "Open Google in Emacs using xwidget-webkit browser."
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
  "Insert parentheses pair and position cursor between them."
  (interactive)
  (insert "()")
  (backward-char))

(defun pb-misc_goto-next-opening-delimiter ()
  "Navigate to the next opening delimiter (parenthesis, bracket, or brace)."
  (interactive)
  (forward-char)
  (re-search-forward "[(\\[\\{]")
  (backward-char))

(defun pb-misc_goto-prev-opening-delimiter ()
  "Navigate to the previous opening delimiter (parenthesis, bracket, or brace)."
  (interactive)
  (re-search-backward "[(\\[\\{]"))

(defun pb-misc_toggle-level-hiding (arg)
  "Toggle hiding at a specific level.
If ARG > 1 or if block is already hidden, show the block then hide at level ARG.
Otherwise, hide the block. Moves cursor one character backward after hiding."
  (interactive "p")
  (hs-life-goes-on
   (if (or (> arg 1) (hs-already-hidden-p))
       (progn (hs-show-block) (hs-hide-level arg))
     (hs-hide-block)))
  (backward-char))

(defun pb-misc_toggle-hiding ()
  "Toggle code block hiding and move cursor back one character."
  (interactive)
  (hs-toggle-hiding)
  (backward-char))

(defun pb-misc_kill-all-dired-buffers ()
  "Kill all `dired' and `dired-sidebar' buffers in one operation.
This function iterates through all existing buffers and kills any
that are in either `dired-mode' or `dired-sidebar-mode'."
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
  "Switch to the *Messages* buffer and move cursor to the end.
This displays the *Messages* buffer in another window and positions
the cursor at the maximum point (end of buffer)."
  (interactive)
  (switch-to-buffer "*Messages*")
  (goto-char (point-max)))

(defun pb-misc_select-vterm-buffer ()
  "Display a list of vterm buffers and switch to the selected one.
Uses consult--read to create an interactive selection menu of all vterm
buffers with live previewing."
  (interactive)
  (let* ((vterm-buffers (seq-filter
                         (lambda (buffer)
                           (with-current-buffer buffer
                             (derived-mode-p 'vterm-mode)))
                         (buffer-list)))
         (buffer-names (mapcar #'buffer-name vterm-buffers))
         (selected (consult--read
                    buffer-names
                    :prompt "Select vterm buffer: "
                    :sort nil
                    :require-match t
                    :category 'buffer
                    :state (consult--buffer-state))))
    (when selected
      (switch-to-buffer selected))))

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
