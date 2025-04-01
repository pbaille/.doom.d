;;; pb/modes/pb-multiline-strings.el -*- lexical-binding: t; -*-

(defface pb-multiline-string/default-face
  '((t :inherit default))
  "Face used to highlight multiline string indentation."
  :group 'pb-multiline-strings)

(defvar-local pb-multiline-strings/indent-face
    'pb-multiline-string/default-face
  "Face to use for indentation of multiline strings.
When nil, no special face is applied to the indentation.")

(defun pb-multiline-strings/indent ()
  "Apply proper indentation to multiline strings using overlays.
Adds a line prefix to each line based on the opening column of the string."
  (interactive)
  (let ((regex "\"\\(?:[^\"\\\\]\\|\\\\.\\)*\""))
    ;; Remove existing indentation overlays first
    (remove-overlays (point-min) (point-max) 'pb-multiline-strings/overlay t)

    ;; Apply indentation to multiline strings
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regex nil t)
        (let ((start (match-beginning 0))
              (end (match-end 0)))
          (when (> (count-lines start end) 1)
            (let* ((start-col (save-excursion
                                (goto-char start)
                                (current-column)))
                   (indent-str (propertize (make-string start-col ?\s) 'face pb-multiline-strings/indent-face))
                   (overlay (make-overlay start end)))
              (overlay-put overlay 'line-prefix indent-str)
              (overlay-put overlay 'pb-multiline-strings/overlay t)
              (overlay-put overlay 'after-string nil))))))))

(defun pb-multiline-strings/unindent ()
  "Remove all indentation overlays from multiline strings."
  (interactive)
  (remove-overlays (point-min) (point-max) 'pb-multiline-strings/overlay t))

(define-minor-mode pb-multiline-strings/mode
  "Minor mode for indenting multiline strings in Emacs Lisp buffers.
When enabled, this mode adds proper indentation to multiline string literals."
  :lighter " IndStr"
  :global nil
  (if pb-multiline-strings/mode
      (progn
        (pb-multiline-strings/indent)
        ;; Refresh indentation when buffer is modified
        (add-hook 'after-change-functions
                  (lambda (&rest _)
                    (when pb-multiline-strings/mode
                      (pb-multiline-strings/indent)))
                  nil t))
    (pb-multiline-strings/unindent)
    (remove-hook 'after-change-functions
                 (lambda (&rest _)
                   (when pb-multiline-strings/mode
                     (pb-multiline-strings/indent)))
                 t)))

(progn :in-place

       (defun pb-multiline-strings/indent-in-place (&optional beg end)
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

       (defun pb-multiline-strings/deindent-in-place ()
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

(provide 'pb-multiline-strings)
