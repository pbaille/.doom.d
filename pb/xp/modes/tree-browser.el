;;; pb/xp/modes/tree-browser.el -*- lexical-binding: t; -*-

;; Define the maximum depth visible in the browser when first created
(defvar-local tree-browser/max-depth 1
  "Maximum depth of the tree to display initially.")

(defvar tree-browser/mode-map
  (make-sparse-keymap)
  "Keymap for `tree-browser/mode'.")

(define-derived-mode tree-browser/mode special-mode "Tree Browser"
  "Major mode for browsing nested km structures.
   \\{tree-browser/mode-map}"
  (setq buffer-read-only t)
  (setq-local truncate-lines t))

(progn :narrowing

       (defvar-local tree-browser/narrow-mode nil
         "When non-nil, moving the cursor in tree browser automatically narrows the source buffer.")

       (defun tree-browser/toggle-narrow-mode ()
         "Toggle narrow mode in the tree browser.
          When enabled, navigating in the tree browser will automatically narrow
          the source buffer to the node at point. When disabled, widen the source buffer."
         (interactive)
         (setq-local tree-browser/narrow-mode (not tree-browser/narrow-mode))
         (if tree-browser/narrow-mode
             (progn
               (message "Narrow mode enabled")
               ;; Apply narrowing immediately when turning the mode on
               (tree-browser/apply-narrowing-at-point))
           ;; Widen the source buffer when disabling narrow mode
           (when-let ((buffer (buffer-local-value 'tree-browser/source-buffer (current-buffer))))
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (widen)
                 (hs-show-all)
                 (when-let ((source-window (get-buffer-window buffer t)))
                   (with-selected-window source-window
                     (recenter)))
                 (message "Source buffer widened"))))
           (message "Narrow mode disabled")))

       (defun tree-browser/apply-narrowing-at-point ()
         "Apply narrowing to the source buffer based on the node at point."
         (when (and tree-browser/narrow-mode
                    (buffer-local-value 'tree-browser/source-buffer (current-buffer)))
           (when-let* ((node-data (get-text-property (line-beginning-position) 'node-data))
                       (start (plist-get node-data :start))
                       (end (plist-get node-data :end))
                       (buffer (buffer-local-value 'tree-browser/source-buffer (current-buffer))))
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 ;; First widen to ensure we're not nested narrowing
                 (widen)
                 ;; Then narrow to the new region
                 (narrow-to-region (save-excursion (goto-char start)
                                                   (beginning-of-line)
                                                   (point))
                                   end)
                 (goto-char start)
                 ;; Make the source buffer window redisplay to show the narrowing
                 (when-let ((source-window (get-buffer-window buffer t)))
                   (with-selected-window source-window
                     (recenter)))))))))

(progn :following

       (defvar-local tree-browser/follow-mode nil
         "When non-nil, navigating in the tree browser will update the source buffer position.")

       (defun tree-browser/toggle-follow-mode ()
         "Toggle follow mode in the tree browser.
          When enabled, navigating in the tree browser will automatically update
          the cursor position in the source buffer."
         (interactive)
         (setq-local tree-browser/follow-mode (not tree-browser/follow-mode))
         (if tree-browser/follow-mode
             (progn
               (message "Follow mode enabled")
               ;; Immediately sync the source with tree browser position
               (tree-browser/sync-source-with-tree))
           (message "Follow mode disabled")))

       ;; Function to sync source buffer with tree browser position
       (defun tree-browser/sync-source-with-tree ()
         "Move cursor in source buffer to match the node at point in tree browser."
         (interactive)
         (when-let* ((node-data (get-text-property (line-beginning-position) 'node-data))
                     (start (plist-get node-data :start))
                     (buffer (buffer-local-value 'tree-browser/source-buffer (current-buffer))))
           (when (buffer-live-p buffer)
             (let ((window (get-buffer-window buffer t)))
               (if window
                   (with-selected-window window
                     (goto-char start)
                     (evil-scroll-line-to-top nil )
                     (symex--update-overlay))
                 (with-current-buffer buffer
                   (goto-char start)))
               (message "Source buffer position updated")))))

       ;; Function for source buffer to sync with tree browser
       (defun tree-browser/sync-tree-with-source ()
         "Sync tree browser position with the current position in source buffer."
         (interactive)
         (when-let ((buffer (buffer-local-value 'tree-browser/source-buffer (current-buffer))))
           (when (buffer-live-p buffer)
             (let ((pos (with-current-buffer buffer (point))))
               (progn
                 (tree-browser/position-cursor-at-node pos)
                 (recenter))
               (when tree-browser/narrow-mode
                 (tree-browser/apply-narrowing-at-point))
               (message "Tree browser position updated"))))))

(progn :actions

       ;; Replace the navigation functions to support follow mode
       (defun tree-browser/next-line ()
         "Move to next line in tree browser and sync with source if follow mode is enabled."
         (interactive)
         (forward-line 1)
         (back-to-indentation)
         (when tree-browser/narrow-mode
           (tree-browser/apply-narrowing-at-point))
         (when tree-browser/follow-mode
           (tree-browser/sync-source-with-tree)))

       (defun tree-browser/prev-line ()
         "Move to previous line in tree browser and sync with source if follow mode is enabled."
         (interactive)
         (forward-line -1)
         (back-to-indentation)
         (when tree-browser/narrow-mode
           (tree-browser/apply-narrowing-at-point))
         (when tree-browser/follow-mode
           (tree-browser/sync-source-with-tree)))

       (defun tree-browser/up-to-parent ()
         "Find the direct parent and put the cursor on it. Sync with source if follow mode is enabled."
         (interactive)
         (when-let* ((node-data (get-text-property (line-beginning-position) 'node-data))
                     (current-path (get-text-property (line-beginning-position) 'tree-path))
                     (parent-path (cdr current-path))) ; Get parent path by removing first element
           (if parent-path
               (progn
                 (tree-browser/goto-path parent-path)
                 (back-to-indentation)
                 (when tree-browser/narrow-mode
                   (tree-browser/apply-narrowing-at-point))
                 (when tree-browser/follow-mode
                   (tree-browser/sync-source-with-tree))
                 (message "Moved to parent node"))
             (message "No parent found for this node"))))

       (defun tree-browser/narrow-node-at-point ()
         "Toggle narrowing in source buffer for the node at point.
          This function maintains compatibility with the old behavior when narrow-mode is off."
         (interactive)
         (if tree-browser/narrow-mode
             ;; In narrow mode, just toggle the mode off
             (progn
               (setq-local tree-browser/narrow-mode nil)
               (when-let ((buffer (buffer-local-value 'tree-browser/source-buffer (current-buffer))))
                 (when (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (widen)
                     (when-let ((source-window (get-buffer-window buffer t)))
                       (with-selected-window source-window
                         (recenter)))
                     (message "Source buffer widened"))))
               (message "Narrow mode disabled"))
           ;; When not in narrow mode, behave like the original function
           (when-let* ((node-data (get-text-property (line-beginning-position) 'node-data))
                       (start (plist-get node-data :start))
                       (end (plist-get node-data :end))
                       (buffer (buffer-local-value 'tree-browser/source-buffer (current-buffer))))
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (if (buffer-narrowed-p)
                     (progn
                       (widen)
                       (when-let ((source-window (get-buffer-window buffer t)))
                         (with-selected-window source-window
                           (recenter)))
                       (message "Source buffer widened"))
                   (narrow-to-region (save-excursion (goto-char start)
                                                     (beginning-of-line)
                                                     (point))
                                     end)
                   (goto-char start)
                   (when-let ((source-window (get-buffer-window buffer t)))
                     (with-selected-window source-window
                       (recenter)))
                   (message "Narrowed source buffer to %s" (or (plist-get node-data :name)
                                                               (plist-get node-data :type)
                                                               "region"))))))))

       (defun tree-browser/scroll-to-node-at-point ()
         "Scroll source buffer to node at point (first line of window)."
         (interactive)
         (when-let* ((node-data (get-text-property (line-beginning-position) 'node-data))
                     (start (plist-get node-data :start))
                     (buffer (buffer-local-value 'tree-browser/source-buffer (current-buffer))))
           (when (buffer-live-p buffer)
             (let ((window (get-buffer-window buffer t)))
               (if window
                   (with-selected-window window
                     (goto-char start)
                     (recenter 0))  ; put at top of window
                 (with-current-buffer buffer
                   (goto-char start)))
               (message "Scrolled to %s" (or (plist-get node-data :name)
                                             (plist-get node-data :type)
                                             "position"))))))

       (defun tree-browser/quit ()
         "Quit the tree browser."
         (interactive)
         (quit-window)
         (widen)
         '(kill-buffer))

       (defun tree-browser/goto-source ()
         "Go to the source location of the node at point."
         (interactive)
         (when-let* ((node-data (get-text-property (line-beginning-position) 'node-data))
                     (start (plist-get node-data :start))
                     (buffer (buffer-local-value 'tree-browser/source-buffer (current-buffer))))
           (let ((should-recenter (not (or tree-browser/follow-mode tree-browser/narrow-mode))))
             (when (buffer-live-p buffer)
               (pop-to-buffer buffer)
               (goto-char start)
               (when should-recenter
                 (recenter)))))))

(progn :depth

       (defun tree-browser/increase-depth ()
         "Increase the maximum depth of the tree display."
         (interactive)
         (let ((current-path (get-text-property (line-beginning-position) 'tree-path)))
           (setq-local tree-browser/max-depth (1+ tree-browser/max-depth))
           (tree-browser/refresh)
           (when current-path
             (tree-browser/goto-path current-path))))

       (defun tree-browser/decrease-depth ()
         "Decrease the maximum depth of the tree display."
         (interactive)
         (when (> tree-browser/max-depth 0)
           (let ((current-path (get-text-property (line-beginning-position) 'tree-path)))
             (setq-local tree-browser/max-depth (1- tree-browser/max-depth))
             (tree-browser/refresh)
             (when current-path
               (tree-browser/goto-path current-path)))))

       (defun tree-browser/goto-path (path)
         "Go to the line containing PATH in the tree browser.
          If PATH is not visible due to depth limitations, find the nearest visible parent."
         (goto-char (point-min))
         ;; Reverse the path since it's stored in reverse order (child to parent)
         (let ((rev-path (reverse path))
               (found nil)
               (line-path nil)
               (depth 0))
           ;; Try to go through each level of the path
           (while (and rev-path (not found) (< depth tree-browser/max-depth))
             (let ((target-prefix (reverse (last rev-path (- (length rev-path) depth)))))
               ;; Search through the buffer for a matching line
               (while (and (not found) (not (eobp)))
                 (setq line-path (get-text-property (line-beginning-position) 'tree-path))
                 ;; Compare the current line's path with our target
                 (when (and line-path
                            (equal (reverse (last (reverse line-path) (length target-prefix)))
                                   target-prefix))
                   (setq found t)
                   (beginning-of-line))
                 (unless found
                   (forward-line 1))))
             ;; If not found, try with a shorter path (parent)
             (unless found
               (setq depth (1+ depth))
               (goto-char (point-min))))

           ;; If we still haven't found anything, just go to the top of the buffer
           (unless found
             (goto-char (point-min))
             ;; Try to find any part of the path
             (while (and (not found) (not (eobp)))
               (setq line-path (get-text-property (line-beginning-position) 'tree-path))
               (when (and line-path
                          (member (car line-path) path))
                 (setq found t)
                 (beginning-of-line))
               (unless found
                 (forward-line 1)))
             ;; If still nothing found, just stay at the top
             (unless found
               (goto-char (point-min)))))))

(progn :buffer-to-tree

       (defun tree-browser/get-treesit-root ()
         "Get the root node of the treesit parser for the current buffer."
         (when (fboundp 'treesit-parser-root-node)
           (when-let ((parser (car (treesit-parser-list))))
             (treesit-parser-root-node parser))))

       (defun tree-browser/remove-package-prefix (name)
         (if-let ((file-name (when (buffer-file-name)
                               (file-name-nondirectory (file-name-sans-extension (buffer-file-name))))))
             (if (string-prefix-p file-name name)
                 (substring name (1+ (length file-name)))
               name)
           name))

       (defun tree-browser/node-tree (node)
         (when node

           (let* ((type (treesit-node-type node))
                  (start (treesit-node-start node))
                  (end (treesit-node-end node))
                  (base (km :type type :start start :end end)))
             (cond ((string= "source_file" type)
                    (km/put base
                            :name (buffer-file-name)
                            :children (seq-keep #'tree-browser/node-tree
                                                (treesit-node-children node))))
                   ((and (string= "special_form" type)
                         (string= "progn" (treesit-node-type (treesit-node-child node 1))))
                    (let ((key (treesit-node-text (treesit-node-child node 2) t)))
                      (km/put base
                              :type "section"
                              :key (intern key)
                              :name (substring key 1)
                              :children (seq-keep #'tree-browser/node-tree
                                                  (sq/butlast
                                                   (sq/drop (treesit-node-children node)
                                                            3))))))
                   ((member type (list "special_form" "list" "function_definition"))
                    (let* ((name-node (treesit-node-child node 2))
                           (name (when (string= "symbol" (treesit-node-type name-node))
                                   (treesit-node-text name-node t))))
                      (km/put base
                              :verb (intern (treesit-node-text (treesit-node-child node 1) t))
                              :name name
                              :short-name (tree-browser/remove-package-prefix name))))
                   (t base)))))

       (pb/comment
        (with-current-buffer "pb-prompt.el"
          (tree-browser/node-tree (tree-browser/get-treesit-root)))))

(progn :tree-render

       (progn :window-handling

              (defvar-local tree-browser/window-width 30
                "The width of the tree browser window.")

              (defun tree-browser/fix-window-size ()
                "Fix the size of the tree browser window."
                (interactive)
                (when-let ((window (get-buffer-window (current-buffer))))
                  (set-window-parameter window 'no-delete-other-windows t)
                  (set-window-parameter window 'no-other-window nil)
                  (set-window-dedicated-p window t)
                  (window-preserve-size window t t) ; preserve width
                  (let ((width tree-browser/window-width))
                    (unless (= (window-width window) width)
                      (adjust-window-trailing-edge window (- width (window-width window)) t)))))

              ;; Ensure window stays fixed after display changes
              (defun tree-browser/enforce-window-width (&rest _)
                "Maintain tree browser window width after window configuration changes."
                (dolist (buffer (buffer-list))
                  (with-current-buffer buffer
                    (when (derived-mode-p 'tree-browser/mode)
                      (when-let ((window (get-buffer-window buffer)))
                        (let ((width tree-browser/window-width))
                          (unless (= (window-width window) width)
                            (adjust-window-trailing-edge window (- width (window-width window)) t))))))))

              ;; Add hook for window configuration changes
              (add-hook 'window-configuration-change-hook 'tree-browser/enforce-window-width))


       (progn :render

              (defun tree-browser/render (tree)
                "Render TREE in the current buffer using tree structure produced by node-tree."
                (let ((inhibit-read-only t))
                  (erase-buffer)
                  (tree-browser/render-node tree 0 () 0)))

              (defun tree-browser/render-node (node depth path current-depth)
                "Render NODE at DEPTH with PATH prefix as the key path at CURRENT-DEPTH level.
                 This function is designed to work with tree structures produced by tree-browser/node-tree."
                (when-let ((name (and (<= current-depth tree-browser/max-depth)
                                      (km? node)
                                      (or (km/get node :short-name)
                                          (km/get node :name)))))
                  ;; Insert the current node
                  (tree-browser/insert-line name depth path node)

                  ;; Process children if any and if we're not at max depth
                  (when-let ((children (and (< current-depth tree-browser/max-depth)
                                            (km/get node :children))))
                    (dolist (child children)
                      (let* ((child-name (km/get child :name))
                             (child-path (cons (pb/string child-name) path)))
                        (tree-browser/render-node child (+ depth 2) child-path (1+ current-depth)))))))

              (defun tree-browser/insert-line (name depth path node-data)
                "Insert a line for DISP-NAME at DEPTH with PATH.
                 Indicate if it HAS-CHILDREN and store NODE-DATA as properties."
                (let ((start (point))
                      (prefix (make-string depth ? )))
                  (insert prefix)

                  ;; Display different symbols based on node type
                  (let ((type (km/get node-data :type)))
                    (cond
                     ((string= type "section")
                      (insert (propertize "* " 'face 'font-lock-keyword-face)))
                     ((member type (list "special_form" "list" "function_definition"))
                      (insert (propertize "• " 'face 'font-lock-doc-face)))
                     (t
                      (insert (propertize "- " 'face 'font-lock-comment-face))))

                    ;; Insert the name with appropriate face
                    (insert (propertize (format "%s" name)
                                        'face (cond
                                               ((string= type "section") 'font-lock-keyword-face)
                                               ((member type (list "function_definition" "special_form" "list"))
                                                'font-lock-comment-face)
                                               (t 'font-lock-comment-face))))

                    (insert "\n")

                    ;; Store path and node data as text properties
                    (put-text-property start (point) 'tree-path path)
                    (put-text-property start (point) 'node-data node-data)))))

       (defun tree-browser/position-cursor-at-node (current-pos)
         "Position cursor at the node that contains CURRENT-POS in the source buffer."
         (when current-pos
           (goto-char (point-min))
           (let ((found-line nil)
                 (best-match nil))
             ;; First pass: find the best matching node
             (while (not (eobp))
               (when-let* ((node-data (get-text-property (line-beginning-position) 'node-data))
                           (start (plist-get node-data :start))
                           (end (plist-get node-data :end)))
                 (when (and (>= current-pos start) (<= current-pos end))
                   ;; If this node contains the point, save it as a potential match
                   (setq found-line (line-number-at-pos))
                   ;; Remember the most specific (smallest) node that contains the point
                   (if (not best-match)
                       (setq best-match (cons start end))
                     (when (< (- end start) (- (cdr best-match) (car best-match)))
                       (setq best-match (cons start end)
                             found-line (line-number-at-pos))))))
               (forward-line 1))

             ;; Go to the best match if found
             (when found-line
               (goto-char (point-min))
               (forward-line (1- found-line))
               (back-to-indentation)))))

       (defun tree-browser/create (tree &optional buffer-name source-buffer)
         "Create a browser for TREE in a new buffer named BUFFER-NAME or *Tree Browser*.
          SOURCE-BUFFER is the original buffer that the tree represents."
         (let* ((buf-name (or buffer-name "*Tree Browser*"))
                (buf-exist (get-buffer buf-name))
                (buf (get-buffer-create buf-name))
                (existing-window (get-buffer-window buf-name))
                (current-pos (when source-buffer
                               (with-current-buffer source-buffer
                                 (point)))))
           (with-current-buffer buf
             (when (not buf-exist)
               (tree-browser/mode)
               (setq-local tree-browser/data tree)
               (setq-local tree-browser/max-depth 1) ;; Default max depth
               (setq-local tree-browser/source-buffer (or source-buffer (current-buffer))))
             (tree-browser/render tree)
             (goto-char (point-min)))

           ;; Only create a new window if one doesn't exist for this buffer
           (unless existing-window
             (let ((width tree-browser/window-width)) ; Width of the tree-browser sidebar
               (let ((window (split-window (selected-window) (- width) 'left)))
                 (select-window window)
                 (switch-to-buffer buf)
                 (set-window-parameter window 'no-delete-other-windows t)
                 (window-preserve-size window t t)
                 (set-window-dedicated-p window t)))) ; Make window dedicated

           ;; If window already exists, just switch to it
           (when existing-window
             (select-window existing-window)
             (tree-browser/fix-window-size))

           ;; Position cursor at the node containing the current position in source buffer
           (tree-browser/position-cursor-at-node current-pos)

           buf))

       (defun tree-browser/navigate-buffer ()
         "Analyze current buffer with treesitter and display as tree."
         (interactive)
         (when-let ((root (tree-browser/get-treesit-root)))
           (let ((tree (tree-browser/node-tree root)))
             (tree-browser/create tree
                                  (format "*Tree Browser: %s*" (buffer-name))
                                  (current-buffer)))))

       ;; Define a new version of the refresh function with more robust functionality
       (defun tree-browser/refresh ()
         "Refresh the tree browser display while preserving current position.
          Maintains selection and expanded state of nodes where possible."
         (interactive)
         (let* ((inhibit-read-only t)
                (current-line (line-number-at-pos))
                (current-path (get-text-property (line-beginning-position) 'tree-path))
                (current-node-data (get-text-property (line-beginning-position) 'node-data)))

           ;; Store the current window's start position
           (let ((window-start-pos (window-start)))
             ;; Redraw the tree structure
             (tree-browser/render tree-browser/data)

             ;; Restore expansion state of previously expanded nodes
             ;; Try to restore position
             (cond
              ;; If we have path data, go to that specific node
              (current-path
               (tree-browser/goto-path current-path))

              ;; Otherwise try to go to the same line number, unless it's now invalid
              (t
               (goto-char (point-min))
               (when (> current-line 1)
                 (forward-line (min (1- current-line)
                                    (1- (count-lines (point-min) (point-max))))))))

             ;; Restore the window start position if possible
             (when (and window-start-pos
                        (< window-start-pos (point-max)))
               (set-window-start (selected-window) window-start-pos))))))

(progn :bindings
       (when (featurep 'evil)
         (evil-define-key 'normal tree-browser/mode-map
           (kbd "j") 'tree-browser/next-line
           (kbd "k") 'tree-browser/prev-line
           (kbd "h") 'tree-browser/up-to-parent
           (kbd "l") 'tree-browser/scroll-to-node-at-point
           (kbd "q") 'tree-browser/quit
           (kbd "RET") 'tree-browser/goto-source
           (kbd ">") 'tree-browser/increase-depth
           (kbd "<") 'tree-browser/decrease-depth
           (kbd "r") 'tree-browser/refresh
           (kbd "n") 'tree-browser/toggle-narrow-mode
           (kbd "f") 'tree-browser/toggle-follow-mode
           (kbd "g g") 'beginning-of-buffer
           (kbd "G") 'end-of-buffer
           (kbd "/") 'isearch-forward)))
