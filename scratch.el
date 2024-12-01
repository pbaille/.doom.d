;;; scratch.el -*- lexical-binding: t; -*-

(setq org-modern-table nil)
(setq org-src-window-setup 'current-window)

(setq symex-refocus-p nil)
(setq lsp-enable-symbol-highlighting nil)

'(progn :docstring-xp

        (defun pb-clojure-adaptive-fill-function ()
          "Clojure adaptive fill function.
This only takes care of filling docstring correctly."
          (when (clojure-in-docstring-p)
            (pp "io")
            (make-string (save-excursion
                           (beginning-of-thing 'string)
                           (- (point)
                              (save-excursion (beginning-of-line) (point))))
                         ? )))

        (setq-local adaptive-fill-function

                    #'pb-clojure-adaptive-fill-function))

'(progn :blink-xp
        (remove-hook 'window-configuration-change-hook 'my-window-change-hook)

        (defun blink-window ()
          "Blink the current window by temporarily changing the background color."
          (interactive)
          (let ((buffer (window-buffer (selected-window))))
            (with-current-buffer buffer
              (let ((remap-cookie (face-remap-add-relative 'default :background "LightSkyBlue1")))
                (run-with-timer 0.2 nil
                                (lambda (buf cookie)
                                  (with-current-buffer buf
                                    (face-remap-remove-relative cookie)))
                                buffer remap-cookie)))))

        (defun my-window-change-hook (frame)
          "Function to run when changing windows (FRAME is ignored)."
          (blink-window))

        (add-hook 'window-selection-change-functions 'my-window-change-hook)
        (remove-hook 'window-selection-change-functions 'my-window-change-hook))



(progn :select-window-for-repl
       (defun my/get-repl-target-windows ()
         "Return a list of window candidates for displaying the REPL, excluding dired-sidebar-mode buffers."
         (seq-filter
          (lambda (win)
            (not (eq 'dired-sidebar-mode (buffer-local-value 'major-mode (window-buffer win)))))
          (window-list)))

       (defun my/cider-choose-window-for-repl ()
         "Select a window with ace-window to display the CIDER REPL, excluding certain buffers."
         (when-let ((repl-buffer (cider-current-repl)))
           (let ((windows (my/get-repl-target-windows)))
             (if (cdr windows)
                 (let ((target-window (aw-select "Select a window for the REPL: " )))
                   (set-window-buffer target-window repl-buffer)
                   (select-window target-window))))))

       (with-eval-after-load 'cider
         (add-hook 'cider-repl-mode-hook #'my/cider-choose-window-for-repl)))

(progn :format-clj
       (defun format-clojure-file-on-save ()
         "Format current clj like file."
         (when (or (string= (file-name-extension buffer-file-name) "clj")
                   (string= (file-name-extension buffer-file-name) "cljs")
                   (string= (file-name-extension buffer-file-name) "edn"))
           (shell-command (concat "standard-clj fix " buffer-file-name))))

       (add-hook 'after-save-hook #'format-clojure-file-on-save))

(progn :audio-export

       (defun my/export-audio-block (code-result)
         "Generate an HTML audio block from the Clojure result."
         (print code-result)
         (let ((mp3-file (gethash :mp3-file (parseedn-read-str code-result))))
           (format "#+BEGIN_EXPORT html\n<audio src=\"%s\" controls>\nYour browser does not support the audio element.\n</audio>\n#+END_EXPORT" mp3-file)))

       (defun my/handle-export-audio (&rest xs)
         "Process and insert audio export blocks before exporting."
         (print xs)
         (org-babel-map-src-blocks nil
           (let* ((info (org-babel-get-src-block-info))
                  (params (nth 2 info))
                  (audio-export (alist-get :export-audio params)))
             (when audio-export
               (let ((result (org-babel-execute-src-block)))
                 ;; Move point to the end of the current block
                 (goto-char (org-babel-where-is-src-block-result))
                 ;; Insert after the result block
                 (forward-line 1)
                 ;; Insert the processed result
                 (insert (concat "\n" (my/export-audio-block result) "\n")))))))

       (add-hook 'org-export-before-processing-hook #'my/handle-export-audio)
       )
