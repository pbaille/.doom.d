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
(require 'symex)
(require 'pb-symex)
(require 'flycheck)

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

(progn :text
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

       (defun pb-misc_remove-leading-spaces (str n)
         "Remove the first N space characters from the beginning of STR."
         (let ((result str)
               (count n))
           (while (and (> count 0) (string-prefix-p " " result))
             (setq result (substring result 1))
             (setq count (1- count)))
           result))

       (defun pb-misc_count-indentation (line)
         "Count leading spaces in a LINE."
         (if (string-match "^\\s-+" line)
             (length (match-string 0 line))
           0))

       (defun pb-misc/query-replace-thing-at-point ()
         (interactive)
         (let* ((current-word (if (bound-and-true-p symex-mode)
                                  (pb-symex_current-as-string)
                                (thing-at-point 'symbol t)))
                (replacement (read-string (concat "Replace '" current-word "' with: " ))))
           (print current-word)
           (when current-word
             (query-replace current-word replacement)))))

(progn :folding
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
         (backward-char)))

(progn :windows

       (defun pb-misc_window-split ()
         (if (> (window-pixel-height) (window-pixel-width))
             (split-window-vertically)
           (split-window-horizontally)))

       (defun pb-misc_dwim-split (&optional buffer)
         "Split the current window and display the previous buffer in the new window.
          This creates a vertical split with the previous buffer displayed in the new window
          while keeping the current buffer in the original window."
         (interactive)
         (let ((new-window (pb-misc_window-split)))
           (set-window-buffer new-window (or buffer (other-buffer)))
           new-window))

       (progn :window-resizing
              (defun pb-misc_increase-window-width ()
                "Increase the width of the current window by half its current width.
                 This function enlarges the window horizontally, making it wider
                 by approximately 50% of its original width."
                (interactive)
                (enlarge-window (/ (window-width) 2) t))

              (defun pb-misc_shrink-window-width ()
                "Decrease the width of the current window by half its current width.
                 This function shrinks the window horizontally, making it narrower
                 by approximately 50% of its original width."
                (interactive)
                (shrink-window (/ (window-width) 2) t))

              (defun pb-misc_increase-window-height ()
                "Increase the height of the current window by half its current height.
                 This function enlarges the window vertically, making it taller
                 by approximately 50% of its original height."
                (interactive)
                (enlarge-window (/ (window-height) 2)))

              (defun pb-misc_shrink-window-height ()
                "Decrease the height of the current window by half its current height.
                 This function shrinks the window vertically, making it shorter
                 by approximately 50% of its original height."
                (interactive)
                (shrink-window (/ (window-height) 2))))

       (defun pb-misc_window-split-consult-buffer ()
         "Split the window and display a selected buffer in the new window.
          This function first splits the window based on its dimensions using
          `pb-misc_window-split`, then prompts the user to select a buffer using
          consult's completion interface. The selected buffer will be displayed
          in the newly created window."
         (interactive)
         ;; if this is aborted we should delete the window
         (let* ((current-buffer (current-buffer))
                (current-window (selected-window))
                (selected (consult--read
                           (mapcar #'buffer-name (buffer-list))
                           :prompt "Select buffer: "
                           :sort t
                           :require-match t
                           :category 'buffer
                           :state (consult--buffer-state)))
                (new-window (pb-misc_dwim-split selected)))
           (switch-to-buffer current-buffer)
           (other-window 1))))

(progn :buffers

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

       (defun pb-misc_scratch-buffer (&optional split)
         "Create or switch to a scratch buffer for the current file or buffer.
          The scratch buffer is named SCRATCH_ followed by the buffer name.
          The scratch buffer will use the same major mode as the current buffer.
          For Emacs Lisp buffers, it will automatically insert a lexical binding header,
          disable flycheck and enable symex-mode.

          When optional argument SPLIT is non-nil, it will split the window
          and display the scratch buffer in the new window."
         (interactive)
         (let* ((scratch-buffer-name (concat "SCRATCH_" (buffer-name)))
                (current-mode major-mode)
                (buffer-exists (get-buffer scratch-buffer-name))
                (buffer (get-buffer-create scratch-buffer-name)))
           (switch-to-buffer buffer)
           (unless buffer-exists
             (funcall current-mode)
             (when (eq major-mode 'emacs-lisp-mode)
               (insert ";;; -*- lexical-binding: t; -*-\n\n()")
               (goto-char (point-max))
               (flycheck-mode -1)
               (symex-mode-interface)))
           (when split
             (pb-misc_dwim-split))))

       (defun pb-misc_new-buffer ()
         "Creates a new buffer prompting user for name and major mode."
         (interactive)
         (let* ((buffer-name (read-string "Buffer name: "))
                (buffer-exists (get-buffer buffer-name)))
           (if buffer-exists
               (switch-to-buffer buffer-exists)
             (let* ((mode-list (all-completions "" obarray
                                                (lambda (s)
                                                  (and (fboundp s)
                                                       (string-match-p "-mode$" (symbol-name s))))))
                    (selected-mode (consult--read mode-list
                                                  :prompt "Major mode: "
                                                  :default "fundamental-mode"
                                                  :require-match t
                                                  :sort t
                                                  :category 'mode))
                    (mode-function (intern selected-mode)))
               (switch-to-buffer (get-buffer-create buffer-name))
               (when (fboundp mode-function)
                 (funcall mode-function))
               (message "Created new buffer '%s' with %s" buffer-name selected-mode)))))

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
       (progn :message-buffer
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
                (goto-char (point-max)))))

(progn :scrolling

       (defun pb-misc/scroll-to-leftmost ()
         "Scroll window to leftmost position without moving cursor."
         (interactive)
         (set-window-hscroll (selected-window) 0))

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
