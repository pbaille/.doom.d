;;; pb/utils/pb-claude.el -*- lexical-binding: t; -*-

(require 'claude-code)

(defun pb-claude/start ()
  "Simplified version of claude-code--start that do not open new window"
  (interactive)
  (let* ((dir (claude-code--directory))
         (default-directory dir)
         ;; Check for existing Claude instances in this directory
         (existing-buffers (claude-code--find-claude-buffers-for-directory dir))
         ;; Get existing instance names
         (existing-instance-names (mapcar (lambda (buf)
                                            (or (claude-code--extract-instance-name-from-buffer-name
                                                 (buffer-name buf))
                                                "default"))
                                          existing-buffers))
         ;; Prompt for instance name (only if instances exist, or force-prompt is true)
         (instance-name (claude-code--prompt-for-instance-name dir existing-instance-names nil))
         (buffer-name (claude-code--buffer-name instance-name))
         (program-switches claude-code-program-switches)

         ;; Set process-adaptive-read-buffering to nil to avoid flickering while Claude is processing
         (process-adaptive-read-buffering nil)

         ;; Start the terminal process
         (buffer (claude-code--term-make claude-code-terminal-backend buffer-name claude-code-program program-switches)))

    ;; Check if the claude program is available
    (unless (executable-find claude-code-program)
      (error "Claude Code program '%s' not found in PATH" claude-code-program))

    ;; Check if buffer was successfully created
    (unless (buffer-live-p buffer)
      (error "Failed to create Claude Code buffer"))

    ;; setup claude buffer
    (with-current-buffer buffer

      ;; Configure terminal with backend-specific settings
      (claude-code--term-configure claude-code-terminal-backend)

      ;; Initialize the window widths hash table
      (setq claude-code--window-widths (make-hash-table :test 'eq :weakness 'key))

      ;; Set up window width tracking if optimization is enabled
      (when claude-code-optimize-window-resize
        (advice-add (claude-code--term-get-adjust-process-window-size-fn claude-code-terminal-backend) :around #'claude-code--adjust-window-size-advice))

      ;; Setup our custom key bindings
      (claude-code--term-setup-keymap claude-code-terminal-backend)

      ;; Customize terminal faces
      (claude-code--term-customize-faces claude-code-terminal-backend)

      ;; remove underlines from _>_
      (face-remap-add-relative 'nobreak-space :underline nil)

      ;; set buffer face
      (buffer-face-set :inherit 'claude-code-repl-face)

      ;; disable scroll bar, fringes
      (setq-local vertical-scroll-bar nil)
      (setq-local fringe-mode 0)

      ;; Add cleanup hook to remove directory mappings when buffer is killed
      (add-hook 'kill-buffer-hook #'claude-code--cleanup-directory-mapping nil t)

      ;; run start hooks
      (run-hooks 'claude-code-start-hook)

      ;; Disable vertical scroll bar in claude buffer
      (setq-local vertical-scroll-bar nil)

      (claude-code-switch-to-buffer))))

(require 'pb-prompt)

(defun pb-claude/send ()
  (interactive)
  (claude-code--do-send-command
   (pb-prompt/mk (km/put (pb-prompt/current-buffer-km)
                         :task (read-string "Task: ")))))

(defun pb-claude/kill ()
  "Kill Claude process and close its window."
  (interactive)
  (if-let ((claude-code-buffer (claude-code--get-or-prompt-for-buffer)))
      (if claude-code-confirm-kill
          (when (yes-or-no-p "Kill Claude instance? ")
            (when-let ((claude-window (get-buffer-window claude-code-buffer)))
              (delete-window claude-window))
            (claude-code--kill-buffer claude-code-buffer)
            (message "Claude instance killed"))
        (when-let ((claude-window (get-buffer-window claude-code-buffer)))
          (delete-window claude-window))
        (claude-code--kill-buffer claude-code-buffer)
        (message "Claude instance killed"))
    (claude-code--show-not-running-message)))

(defun pb-claude/toggle-window ()
  "If claude buffer window is visible close it else use claude switch to buffer command"
  (interactive)
  (if-let ((claude-code-buffer (claude-code--get-or-prompt-for-buffer)))
      (if-let ((claude-window (get-buffer-window claude-code-buffer)))
          (delete-window claude-window)
        (claude-code-switch-to-buffer))
    (claude-code--show-not-running-message)))

(defun pb-claude/dwim ()
  (interactive)
  (let* ((dir (claude-code--directory))
         (default-directory dir)
         ;; Check for existing Claude instances in this directory
         (existing-buffers (claude-code--find-claude-buffers-for-directory dir)))
    (if existing-buffers
        (pb-claude/toggle-window)
      (pb-claude/start))))

(provide 'pb-claude)
