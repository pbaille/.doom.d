;;; pb/utils/pb-format.el -*- lexical-binding: t; -*-

(progn :multiline-strings

       (defun pb-format/indent-strings (&optional beg end)
         "Indent multiline strings by modifying buffer text.
          Adds proper indentation at the beginning of each line of a multiline string
          based on the opening column of the string.

          When called interactively or when BEG and END are nil, operate on the entire buffer.
          Otherwise, only operate on the region from BEG to END."
         (interactive)
         (save-excursion
           (let ((beg (or beg (point-min)))
                 (end (or end (point-max)))
                 (regex "\"\\(?:[^\"\\\\]\\|\\\\.\\)*\"")
                 (modified-p nil))
             ;; Apply indentation to multiline strings
             (goto-char beg)
             (while (and (< (point) end)
                         (re-search-forward regex end t))
               (let ((str-start (match-beginning 0))
                     (str-end (match-end 0)))
                 (when (> (count-lines str-start str-end) 1)
                   (let* ((start-col (save-excursion
                                       (goto-char str-start)
                                       (current-column)))
                          (indent-str (make-string (1+ start-col) ?\s)))

                     ;; Go through each line of the multiline string
                     (save-excursion
                       (goto-char str-start)
                       (forward-line 1)
                       (while (< (point) str-end)
                         (let ((line-start (point)))
                           ;; Check if line needs indentation
                           (when (and (< line-start str-end)
                                      (looking-at "\\s-*"))
                             (let ((current-indent (length (match-string 0))))
                               ;; Only modify if the indentation is different
                               (unless (= current-indent start-col)
                                 (delete-region line-start (+ line-start current-indent))
                                 (insert indent-str)
                                 (setq modified-p t)
                                 ;; Update end position after modification
                                 (setq str-end (+ str-end (- start-col current-indent)))
                                 ;; Also update the overall region end if needed
                                 (when (> str-end end)
                                   (setq end str-end))))))
                         (forward-line 1)))))))

             (when modified-p
               (message "Multiline strings have been indented in place.")))))

       (defun pb-format/deindent-strings ()
         "Remove indentation from multiline strings by modifying buffer text.
          Removes any indentation at the beginning of each line of a multiline string."
         (interactive)
         (save-excursion
           (let ((regex "\"\\(?:[^\"\\\\]\\|\\\\.\\)*\"")
                 (modified-p nil))
             ;; Remove indentation from multiline strings
             (goto-char (point-min))
             (while (re-search-forward regex nil t)
               (let ((start (match-beginning 0))
                     (end (match-end 0)))
                 (when (> (count-lines start end) 1)
                   ;; Go through each line of the multiline string
                   (save-excursion
                     (goto-char start)
                     (forward-line 1)
                     (while (< (point) end)
                       (let ((line-start (point)))
                         ;; Remove all indentation at beginning of line
                         (when (and (< line-start end)
                                    (looking-at "\\s-+"))
                           (let ((current-indent (length (match-string 0))))
                             (delete-region line-start (+ line-start current-indent))
                             (setq modified-p t)
                             ;; Update end position after modification
                             (setq end (- end current-indent)))))
                       (forward-line 1))))))

             (when modified-p
               (message "Indentation has been removed from multiline strings."))))))

(defun pb-format/well-formed-lisp-buffer-p ()
  "Check if the current buffer has well balanced parens.
   Raises an error and goes to the position if unbalanced."
  (interactive)
  (save-excursion
    (condition-case err
        (progn
          (goto-char (point-min))
          (scan-sexps (point-min) (point-max))
          (when (called-interactively-p 'any)
            (message "Balanced expressions: OK"))
          t)
      (scan-error
       (goto-char (nth 2 err)) ; Go to the error position
       (let ((error-msg (format "Unbalanced expression at line %d: %s"
                                (line-number-at-pos)
                                (error-message-string err))))
         (when (called-interactively-p 'any)
           (pulse-momentary-highlight-one-line (point)))
         (error error-msg))))))

(defvar pb-format/lisp-modes
  '(clojure-mode clojurescript-mode clojurec-mode
    emacs-lisp-mode fennel-mode scheme-mode racket-mode))

(defun pb-format/save-lisp-buffer ()
  "Save the current lisp buffer.
   This function checks if the current buffer is in a lisp mode and has a file name,
   then performs the following operations:
   1. Checks that the buffer has well-balanced parentheses
   2. Indents the entire buffer
   3. Properly indents multiline strings
   4. Saves the buffer

   If any errors occur during formatting (especially unbalanced expressions),
   the operation is canceled with an appropriate error message."
  (when (and (memq major-mode pb-format/lisp-modes)
             (buffer-file-name))
    (condition-case err
        (progn
          (pb-format/well-formed-lisp-buffer-p)
          (indent-region (point-min) (point-max))
          (pb-format/indent-strings)
          (save-buffer))
      (error
       (message "Save canceled: %s" (error-message-string err))
       (pulse-momentary-highlight-one-line (point))
       (sit-for 1)
       nil))))

(provide 'pb-format)
