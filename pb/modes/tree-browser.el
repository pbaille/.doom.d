;;; pb/xp/modes/tree-browser.el -*- lexical-binding: t; -*-

(require 'evil)

;; Define the maximum depth visible in the browser when first created
(defvar-local tree-browser/max-depth 1
  "Maximum depth of the tree to display initially.")

(defvar-local tree-browser/data nil
  "The tree data structure being displayed in this browser instance.")

(defvar tree-browser/mode-map
  (make-sparse-keymap)
  "Keymap for `tree-browser/mode'.")

(define-derived-mode tree-browser/mode special-mode "Tree Browser"
  "Major mode for browsing nested km structures.
   \\{tree-browser/mode-map}"
  (setq buffer-read-only t)
  (setq-local truncate-lines t))

(progn :depth

       (defun tree-browser/increase-depth ()
         "Increase the maximum depth of the tree display."
         (interactive)
         (let ((current-path (get-text-property (line-beginning-position) 'tree-path))
               (current-line (line-number-at-pos))
               (window-start-line (line-number-at-pos (window-start))))
           (setq-local tree-browser/max-depth (1+ tree-browser/max-depth))
           (tree-browser/refresh)
           (when current-path
             (tree-browser/goto-path current-path))
           ;; Try to maintain the cursor's relative position in the visible window
           (back-to-indentation)
           '(evil-scroll-line-to-top (1- window-start-line))
           ))

       (defun tree-browser/decrease-depth ()
         "Decrease the maximum depth of the tree display.
          If the current node would become invisible, move to its visible parent.
          Syncs with source buffer after decreasing depth."
         (interactive)
         (when (> tree-browser/max-depth 0)
           (let* ((current-path (get-text-property (line-beginning-position) 'tree-path))
                  ;; Calculate a visible parent path that won't exceed the new max depth
                  (visible-path (when current-path
                                  (let ((path-length (length current-path))
                                        (new-depth (1- tree-browser/max-depth)))
                                    (if (> path-length new-depth)
                                        ;; Take only the elements that will be visible at new depth
                                        (nthcdr (- path-length new-depth) current-path)
                                      current-path)))))
             (setq-local tree-browser/max-depth (1- tree-browser/max-depth))
             (tree-browser/refresh)
             (when visible-path
               (tree-browser/goto-path visible-path))
             (back-to-indentation)
             ;; Sync with source buffer after decreasing depth
             (when tree-browser/narrow-mode
               (tree-browser/apply-narrowing-at-point))
             (when tree-browser/follow-mode
               (tree-browser/sync-source-with-tree)))))

       (defun tree-browser/goto-path (path)
         "Go to the line containing PATH in the tree browser.
          If PATH is not visible due to depth limitations, find the nearest visible parent.
          Ensures the line is scrolled into view."
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
               (goto-char (point-min))))

           ;; Make sure the line is visible in the window
           (when (get-buffer-window (current-buffer))
             (recenter)))))

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
                     ;; Apply recenter based on centered mode setting

                     (symex--update-overlay)))))))))

(progn :following

       (defvar-local tree-browser/follow-mode t
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
         "Move cursor in source buffer to match the node at point in tree browser.
          When centered-mode is enabled, the node will be centered in the window.
          Otherwise, the node will be positioned at the top of the window."
         (interactive)
         (when-let* ((node-data (get-text-property (line-beginning-position) 'node-data))
                     (start (plist-get node-data :start))
                     (buffer (buffer-local-value 'tree-browser/source-buffer (current-buffer))))
           (let ((centered tree-browser/centered-mode)
                 (tree-browser-current-line (- (line-number-at-pos)
                                               (line-number-at-pos (window-start)))))
             (when (buffer-live-p buffer)
               (let ((window (get-buffer-window buffer t)))
                 (if window
                     (with-selected-window window
                       (goto-char start)
                       (symex--update-overlay)
                       ;; Always call evil-scroll-line-to-top first to ensure proper positioning
                       (evil-scroll-line-to-top nil)
                       ;; Apply centering only if centered mode is enabled
                       (when centered
                         '(recenter) ;; try to center on the current tree-browser line
                         (evil-scroll-line-to-top (1+ (- (line-number-at-pos)
                                                         tree-browser-current-line)))))
                   (with-current-buffer buffer
                     (goto-char start)))
                 (message "Source buffer position updated"))))))

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

(progn :centered

       (defvar-local tree-browser/centered-mode nil
         "When non-nil, keep the current node centered in the source buffer.
          This works in conjunction with follow-mode and narrow-mode.")

       (defun tree-browser/toggle-centered-mode ()
         "Toggle centered mode in the tree browser.
          When enabled, the current node will be centered in the source buffer
          when using follow-mode or narrow-mode."
         (interactive)
         (setq-local tree-browser/centered-mode (not tree-browser/centered-mode))
         (message "Centered mode %s" (if tree-browser/centered-mode "enabled" "disabled"))
         ;; Apply appropriate positioning immediately
         (when-let* ((node-data (get-text-property (line-beginning-position) 'node-data))
                     (start (plist-get node-data :start))
                     (buffer (buffer-local-value 'tree-browser/source-buffer (current-buffer))))
           (let ((centered tree-browser/centered-mode))
             (when (buffer-live-p buffer)
               (let ((window (get-buffer-window buffer t)))
                 (when window
                   (with-selected-window window
                     (goto-char start)
                     (if centered
                         (recenter) ;; Center in window when enabling centered mode
                       (recenter 0)) ;; Put at top of window when disabling
                     (symex--update-overlay))))))))

       (defun tree-browser/apply-centering-at-point ()
         "Center the current node in the source buffer if centered-mode is enabled."
         (when (and tree-browser/centered-mode
                    (buffer-local-value 'tree-browser/source-buffer (current-buffer)))
           (when-let* ((node-data (get-text-property (line-beginning-position) 'node-data))
                       (start (plist-get node-data :start))
                       (buffer (buffer-local-value 'tree-browser/source-buffer (current-buffer))))
             (when (buffer-live-p buffer)
               (let ((window (get-buffer-window buffer t)))
                 (when window
                   (with-selected-window window
                     (goto-char start)
                     (recenter 0)  ;; Put at top of window
                     (symex--update-overlay)))))))))

(progn :search

       (defvar-local tree-browser/search-term nil
         "Current search term used for filtering nodes in the tree browser.")

       (defvar-local tree-browser/original-data nil
         "Original tree data before filtering.")

       (defvar-local tree-browser/highlight-overlay nil
         "Overlay used to highlight the current node in the tree browser.")

       (defun tree-browser/filter-tree (tree search-term)
         "Filter TREE to include only nodes that match SEARCH-TERM or their ancestors.
          Returns the filtered tree structure."
         (when tree
           (let ((matches-or-has-children nil)
                 (filtered-children '())
                 (node-text (or (and (km? tree) (or (km/get tree :name)
                                                    (km/get tree :short-name)))
                                "")))

             ;; Check if this node matches
             (let ((node-matches (and (stringp node-text)
                                      (string-match-p (regexp-quote search-term)
                                                      (downcase (substring-no-properties node-text))))))

               ;; Process children if any
               (when-let ((children (and (km? tree) (km/get tree :children))))
                 (setq filtered-children
                       (delq nil (mapcar (lambda (child)
                                           (tree-browser/filter-tree child search-term))
                                         children)))
                 (when filtered-children
                   (setq matches-or-has-children t)))

               ;; Include this node if it matches or has matching descendants
               (when (or node-matches matches-or-has-children)
                 (if (km? tree)
                     (km/put (copy-tree tree) :children filtered-children)
                   tree))))))

       (defun tree-browser/clear-search ()
         "Clear the current search filter and restore the original tree view."
         (interactive)
         (when tree-browser/original-data
           (setq-local tree-browser/data tree-browser/original-data)
           (setq-local tree-browser/original-data nil)
           (setq-local tree-browser/search-term nil)
           (tree-browser/refresh)
           (message "Search cleared")))

       (defun tree-browser/live-search ()
         "Interactively search and filter tree browser nodes as you type.
          Shows matching nodes and their ancestors in real-time.
          If search is aborted with C-g, restores original tree state and closes tree browser."
         (interactive)
         (let ((minibuffer-setup-hook
                (cons (lambda ()
                        ;; Save original data on first search
                        (unless tree-browser/original-data
                          (setq-local tree-browser/original-data tree-browser/data))

                        ;; Setup navigation keybindings in minibuffer
                        (let ((map (make-sparse-keymap)))
                          (set-keymap-parent map (current-local-map))
                          (define-key map (kbd "C-j") 'tree-browser/search-next-match)
                          (define-key map (kbd "C-k") 'tree-browser/search-prev-match)
                          (use-local-map map))

                        ;; Setup live updating
                        (add-hook 'post-command-hook 'tree-browser/live-update-filter nil t))
                      minibuffer-setup-hook))
               (current-term tree-browser/search-term)
               (was-aborted nil)
               (source-buffer (buffer-local-value 'tree-browser/source-buffer (current-buffer))))

           ;; Read the search term with live updates
           (unwind-protect
               (condition-case nil
                   (setq tree-browser/search-term
                         (read-string "Filter nodes (live): " current-term))
                 (quit
                  ;; Handle C-g abort case
                  (setq was-aborted t)
                  (when tree-browser/original-data
                    (setq-local tree-browser/data tree-browser/original-data)
                    (setq-local tree-browser/original-data nil)
                    (setq-local tree-browser/search-term nil)
                    (tree-browser/refresh)
                    (tree-browser/remove-highlight)
                    (message "Search aborted")
                    ;; Use tree-browser/quit instead of manual buffer/window management
                    (tree-browser/quit))))

             ;; Cleanup hook and highlight when done (happens regardless of completion or abort)
             (when (active-minibuffer-window)
               (with-current-buffer (window-buffer (active-minibuffer-window))
                 (remove-hook 'post-command-hook 'tree-browser/live-update-filter t)))
             
             ;; Remove highlight after search completes
             (tree-browser/remove-highlight))

           ;; Only process search results if search wasn't aborted
           (unless was-aborted
             ;; Handle empty search term (clear search)
             (if (string-empty-p tree-browser/search-term)
                 (tree-browser/clear-search)
               (message "Showing matches for \"%s\" (press / to change, ESC to clear)"
                        tree-browser/search-term)))))

       (defun tree-browser/live-update-filter ()
         "Update the tree browser filter based on current minibuffer content.
          This function is called after each keystroke in the minibuffer."
         (let ((current-input (minibuffer-contents)))
           (with-selected-window (minibuffer-selected-window)
             (when (derived-mode-p 'tree-browser/mode)
               ;; Ensure we have original data to filter
               (unless tree-browser/original-data
                 (setq-local tree-browser/original-data tree-browser/data))

               ;; Apply filter with current input
               (if (string-empty-p current-input)
                   ;; Show original data if search is empty
                   (progn
                     (setq-local tree-browser/data tree-browser/original-data)
                     (tree-browser/remove-highlight))
                 ;; Otherwise filter the tree
                 (let ((filtered-tree (tree-browser/filter-tree
                                       tree-browser/original-data
                                       current-input)))
                   (when filtered-tree
                     (setq-local tree-browser/data filtered-tree)
                     (setq-local tree-browser/max-depth 99))))

               ;; Refresh display
               (tree-browser/refresh)
               
               ;; Highlight current line if we have search results
               (unless (string-empty-p current-input)
                 (tree-browser/highlight-current-line))))))

       (defun tree-browser/highlight-current-line ()
         "Highlight the current line in the tree browser."
         (when (derived-mode-p 'tree-browser/mode)
           ;; Remove existing highlight
           (when tree-browser/highlight-overlay
             (delete-overlay tree-browser/highlight-overlay))
           
           ;; Create new highlight overlay
           (let ((start (line-beginning-position))
                 (end (1+ (line-end-position))))
             (setq tree-browser/highlight-overlay (make-overlay start end))
             (overlay-put tree-browser/highlight-overlay 'face 
                          '(:inherit hl-line :extend t))
             (overlay-put tree-browser/highlight-overlay 'priority 100))))

       (defun tree-browser/remove-highlight ()
         "Remove the current line highlight in the tree browser."
         (when tree-browser/highlight-overlay
           (delete-overlay tree-browser/highlight-overlay)
           (setq tree-browser/highlight-overlay nil)))

       (defun tree-browser/search-next-match ()
         "Navigate to the next matched node in the tree browser during live search."
         (interactive)
         (with-selected-window (minibuffer-selected-window)
           (when (derived-mode-p 'tree-browser/mode)
             (forward-line 1)
             (back-to-indentation)
             (tree-browser/highlight-current-line)
             (when tree-browser/narrow-mode
               (tree-browser/apply-narrowing-at-point))
             (when tree-browser/follow-mode
               (tree-browser/sync-source-with-tree)))))

       (defun tree-browser/search-prev-match ()
         "Navigate to the previous matched node in the tree browser during live search."
         (interactive)
         (with-selected-window (minibuffer-selected-window)
           (when (derived-mode-p 'tree-browser/mode)
             (forward-line -1)
             (back-to-indentation)
             (tree-browser/highlight-current-line)
             (when tree-browser/narrow-mode
               (tree-browser/apply-narrowing-at-point))
             (when tree-browser/follow-mode
               (tree-browser/sync-source-with-tree))))))

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

       (defun tree-browser/yank-node ()
         "Copy the current node's content to the kill ring.
          This function extracts the text of the node at point from the source buffer
          and adds it to the kill ring, making it available for pasting elsewhere."
         (interactive)
         (when-let* ((node-data (get-text-property (line-beginning-position) 'node-data))
                     (start (plist-get node-data :start))
                     (end (plist-get node-data :end))
                     (buffer (buffer-local-value 'tree-browser/source-buffer (current-buffer))))
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (let ((text (buffer-substring-no-properties start end)))
                 (kill-new text)
                 (message "Copied %s to kill ring (%d characters)"
                          (or (plist-get node-data :name)
                              (plist-get node-data :type)
                              "node")
                          (length text)))))))

       (defun tree-browser/insert-after-node ()
         "Close the tree browser and position cursor for editing after the current node.
          This function:
          1. Gets the current node position data
          2. Closes the tree browser
          3. Navigates to the end of the node in the source buffer
          4. Inserts a newline
          5. Enters insert mode, positioning cursor for immediate editing"
         (interactive)
         (when-let* ((node-data (get-text-property (line-beginning-position) 'node-data))
                     (end (plist-get node-data :end))
                     (buffer (buffer-local-value 'tree-browser/source-buffer (current-buffer))))
           (when (buffer-live-p buffer)
             (tree-browser/quit)
             (let ((current-node-indent (save-excursion
                                          (goto-char (km/get node-data :start))
                                          (current-column))))
               (with-current-buffer buffer
                 (goto-char end)
                 (insert "\n")
                 (when (fboundp 'evil-insert-state)
                   (evil-insert-state)
                   (indent-to current-node-indent)))))))

       (defun tree-browser/delete-node ()
         "Delete node at point with confirmation.
          This function deletes the node at point from the source buffer after
          confirming with the user. It updates both the tree browser and source buffer."
         (interactive)
         (when-let* ((node-data (get-text-property (line-beginning-position) 'node-data))
                     (start (plist-get node-data :start))
                     (end (plist-get node-data :end))
                     (buffer (buffer-local-value 'tree-browser/source-buffer (current-buffer)))
                     (node-name (or (plist-get node-data :name)
                                    (plist-get node-data :type)
                                    "unnamed node")))
           (when (and (buffer-live-p buffer)
                      (yes-or-no-p (format "Delete %s? " node-name)))
             (with-current-buffer buffer
               (delete-region start end)
               (when (looking-at "[ \t\n]+")
                 (delete-region (point) (match-end 0))))
             (when-let ((root (with-current-buffer buffer
                                (tree-browser/get-treesit-root))))
               (setq-local tree-browser/data (with-current-buffer buffer
                                               (tree-browser/node-tree root))))
             (tree-browser/refresh)
             (message "Deleted %s" node-name))))

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
         (let ((buf (current-buffer)))
           ;; Clean up highlight overlay before quitting
           (tree-browser/remove-highlight)
           (if tree-browser/source-buffer
               (progn (quit-window)
                      (kill-buffer buf)
                      (widen)
                      (balance-windows))
             (let ((tb-buffers (cl-remove-if-not
                                (lambda (b)
                                  (with-current-buffer b
                                    (derived-mode-p 'tree-browser/mode)))
                                (buffer-list))))
               (if tb-buffers
                   (dolist (tb-buf tb-buffers)
                     (when-let ((win (get-buffer-window tb-buf)))
                       (with-selected-window win
                         (quit-window))
                       (kill-buffer tb-buf)))
                 ;; If we can't find any tree browser buffers, just quit the window
                 (quit-window))))))

       (defun tree-browser/goto-source (&optional close-browser)
         "Go to the source location of the node at point.
          With prefix argument CLOSE-BROWSER, close the tree browser after navigation."
         (interactive "P")
         (when-let* ((node-data (get-text-property (line-beginning-position) 'node-data))
                     (start (plist-get node-data :start))
                     (buffer (buffer-local-value 'tree-browser/source-buffer (current-buffer))))
           (let ((should-recenter (not (or tree-browser/follow-mode tree-browser/narrow-mode)))
                 (browser-buffer (current-buffer)))
             (when close-browser
               (with-current-buffer browser-buffer
                 (kill-buffer-and-window)))
             (when (buffer-live-p buffer)
               (pop-to-buffer buffer)
               (goto-char start)
               (when should-recenter
                 (recenter))
               (when (and (eq major-mode 'org-mode)
                          (string= "src-block" (km/get node-data :type)))
                 (sorg/edit-block)))
             (balance-windows))))

       (defun tree-browser/open-dired-sidebar ()
         "Close current tree-browser and open dired-sidebar focusing source file."
         (interactive)
         (tree-browser/quit)
         (dired-sidebar-toggle-sidebar))

       (defun tree-browser/eval ()
         "Evaluate the node at point in the tree browser.
          For Emacs Lisp nodes, evaluates the code and displays results.
          For other languages or node types, attempts to use appropriate evaluation method.
          Results are displayed in a results buffer or message area depending on size."
         (interactive)
         (when-let* ((node-data (get-text-property (line-beginning-position) 'node-data))
                     (start (plist-get node-data :start))
                     (end (plist-get node-data :end))
                     (buffer (buffer-local-value 'tree-browser/source-buffer (current-buffer))))
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (let* ((node-type (plist-get node-data :type)))

                 ;; Handle evaluation based on node type or major mode
                 (cond
                  ;; For Emacs Lisp code
                  ((eq major-mode 'emacs-lisp-mode)
                   (symex--evaluate)
                   (save-excursion
                     (goto-char start)
                     (symex--evaluate)))

                  ;; For org-mode src blocks
                  ((and (eq major-mode 'org-mode)
                        (string= node-type "src-block"))
                   (save-excursion
                     (goto-char start)
                     (sorg/eval-current-block)))

                  ;; Default case - can't evaluate
                  (t (message "Don't know how to evaluate node of type: %s" node-type))))))))

       (progn :query

              (require 'pb-prompt)
              (require 'pb-meta)

              (defun tree-browser/query ()
                "Query LLM about the node at point using pb-prompt.
                 Offers choice between inline response or creating an org-chat."
                (interactive)
                (when-let* ((node-data (get-text-property (line-beginning-position) 'node-data))
                            (start (plist-get node-data :start))
                            (end (plist-get node-data :end))
                            (buffer (buffer-local-value 'tree-browser/source-buffer (current-buffer))))
                  (let* ((node-name (or (plist-get node-data :name)
                                        (plist-get node-data :type)
                                        "unnamed-node"))
                         (source-content (with-current-buffer buffer
                                           (buffer-substring-no-properties start end)))
                         (query-text (read-string (format "Query about '%s': " node-name)))
                         (mode (completing-read "Response mode: " '("inline" "org-chat") nil t)))
                    (if (string= mode "inline")
                        (progn (tree-browser/quit)
                               (pb-prompt/query (km :instructions query-text)))
                      (tree-browser/query-org-chat buffer node-name source-content query-text)))))

              (defun tree-browser/query-org-chat (buffer node-name source-content query-text)
                "Create an org-chat for the query in a meta file."
                (with-current-buffer buffer
                  (let* ((file-name (buffer-file-name))
                         (chat-file (pb-meta/-get-file-meta-dir file-name))
                         (buffer-mode (symbol-name major-mode))
                         (chat-filename (concat (file-name-sans-extension
                                                 (concat (replace-regexp-in-string "[^a-zA-Z0-9-_.]" "-" node-name) "-chat-"
                                                         (format-time-string "%Y%m%d-%H%M%S")))
                                                ".org"))
                         (full-path (expand-file-name chat-filename chat-file)))

                    ;; Create meta directory if needed
                    (unless (file-exists-p chat-file)
                      (make-directory chat-file t))

                    ;; Add original source buffer to context
                    (pb-prompt/add-item!
                     (km :type "buffer"
                         :path file-name
                         :buffer-name (buffer-name)
                         :major-mode buffer-mode
                         :content source-content))

                    ;; Create and set up the chat file
                    (with-current-buffer (find-file-noselect full-path)
                      (erase-buffer)
                      (org-mode)

                      ;; Insert chat content
                      (insert (format "#+TITLE: Chat about: %s\n\n" node-name))
                      (insert "* Context\n\n")
                      (insert "#+begin_src " (substring buffer-mode 0 (string-match "-mode$" buffer-mode)) "\n")
                      (insert source-content)
                      (insert "\n#+end_src\n\n")
                      (insert "* Chat\n\n")
                      (insert "** User\n\n")
                      (insert query-text)
                      (insert "\n\n** Assistant\n\n")

                      ;; Show the buffer and position cursor on the Assistant header
                      (pop-to-buffer (current-buffer))

                      ;; Position cursor on Assistant header and enable sorg mode
                      (goto-char (point-max))
                      (evil-sorg-state 1)

                      ;; Setup for sorg/query-replace
                      (pb-prompt/query (km :instructions query-text))

                      (message "Created org chat at %s" full-path))))))
       )

(progn :mouse-support

       (defun tree-browser/mouse-click (event)
         "Handle mouse click EVENT in the tree browser.
          When a node is clicked, focus that node in the source buffer."
         (interactive "e")
         (let ((pos (posn-point (event-end event))))
           (when pos
             (goto-char pos)
             (back-to-indentation)
             ;; Sync with source buffer
             (when tree-browser/narrow-mode
               (tree-browser/apply-narrowing-at-point))
             (tree-browser/sync-source-with-tree)
             ;; Focus the window with the source buffer
             (when-let ((buffer (buffer-local-value 'tree-browser/source-buffer (current-buffer)))
                        (window (get-buffer-window buffer t)))
               (select-window window)))))

       ;; Add mouse bindings
       (define-key tree-browser/mode-map [mouse-1] 'tree-browser/mouse-click)
       (define-key tree-browser/mode-map [down-mouse-1] nil))

(progn :buffer-to-tree

       (defun tree-browser/get-treesit-root ()
         "Get the root node of the treesit parser for the current buffer."
         (when (fboundp 'treesit-parser-root-node)
           (when-let ((parser (car (treesit-parser-list))))
             (treesit-parser-root-node parser))))

       (defun tree-browser/package-prefixed? (name)
         "Return non-nil if NAME starts with the current file's package prefix."
         (when-let ((file-name (when (buffer-file-name)
                                 (file-name-nondirectory (file-name-sans-extension (buffer-file-name))))))
           (string-prefix-p file-name name)))

       (defun tree-browser/remove-package-prefix (name)
         (if (tree-browser/package-prefixed? name)
             (let ((file-name (file-name-nondirectory (file-name-sans-extension (buffer-file-name)))))
               (if (and (> (length name) (length file-name))
                        (char-equal (aref name (length file-name)) ?/))
                   (substring name (1+ (length file-name)))
                 name))
           name))

       (defun tree-browser/node-tree (node &optional level)
         (when node
           (let* ((type (treesit-node-type node))
                  (start (treesit-node-start node))
                  (end (treesit-node-end node))
                  (level (or level 1))
                  (base (km :type type :start start :end end :level level)))
             (cond ((string= "source_file" type)
                    (km/put base
                            :name (file-name-nondirectory (buffer-file-name))
                            :children (seq-keep (lambda (node)
                                                  (tree-browser/node-tree node (1+ level)))
                                                (treesit-node-children node))))
                   ((and (string= "special_form" type)
                         (string= "progn" (treesit-node-type (treesit-node-child node 1))))
                    (let ((key (treesit-node-text (treesit-node-child node 2) t)))
                      (km/put base
                              :type "section"
                              :key (intern key)
                              :name (substring key 1)
                              :children (seq-keep (lambda (node)
                                                    (tree-browser/node-tree node (1+ level)))
                                                  (sq/butlast
                                                   (sq/drop (treesit-node-children node)
                                                            3))))))
                   ((member type (list "special_form" "list" "function_definition"))
                    (let* ((name-node (treesit-node-child node 2))
                           (verb-node (treesit-node-child node 1))
                           (verb (when verb-node (intern (treesit-node-text verb-node t))))
                           (name (when (member (treesit-node-type name-node) '("symbol" "quote"))
                                   (if (string= (treesit-node-type name-node) "quote")
                                       (treesit-node-text (treesit-node-child name-node 1) t)
                                     (treesit-node-text name-node t))))
                           (is-var-def (member verb '(defvar defvar-local)))
                           (is-fun-def (eq verb 'defun)))
                      (km/merge (km/put base
                                        :verb verb
                                        :var-def is-var-def
                                        :fun-def is-fun-def)
                                (if (or is-fun-def is-var-def)
                                    (km :name name
                                        :short-name (tree-browser/remove-package-prefix name))
                                  (km :package-prefixed? (tree-browser/package-prefixed? name)
                                      :second-name (tree-browser/remove-package-prefix name))))))
                   (t base)))))

       (defun tree-browser/file->node-tree (path)
         "Create a node tree for a file at PATH using treesit.
          Returns a tree structure or nil if the file cannot be parsed."
         (when (and path (file-exists-p path))
           (let ((buffer (find-file-noselect path)))
             (with-current-buffer buffer
               (prog1
                   (when-let ((root (tree-browser/get-treesit-root)))
                     (tree-browser/node-tree root))
                 ;; Clean up the temporary buffer if we created one
                 (unless (get-buffer-window buffer)
                   (kill-buffer buffer)))))))

       (defun tree-browser/definitions-nodes (node-tree)
         "Walk the NODE-TREE and return a list of definition nodes.
          Each returned node contains :name, :start, and :end properties.
          Collects function and variable definitions from Lisp code."
         (when node-tree
           (let ((result nil))
             ;; Check if current node is a definition
             (when (and (km? node-tree)
                        (or (km/get node-tree :fun-def)
                            (km/get node-tree :var-def))
                        (km/get node-tree :name)
                        (km/get node-tree :start)
                        (km/get node-tree :end))
               (push (km :name (km/get node-tree :name)
                         :start (km/get node-tree :start)
                         :end (km/get node-tree :end)
                         :type (if (km/get node-tree :fun-def) "function" "variable"))
                     result))

             ;; Recursively process children
             (when-let ((children (and (km? node-tree) (km/get node-tree :children))))
               (dolist (child children)
                 (setq result (append (tree-browser/definitions-nodes child) result))))

             ;; Return collected definitions
             result)))

       (defun tree-browser/file-definitions (path)
         "Return list of function and variable definitions from file at PATH.
          Each item contains :name, :start, :end and :type properties.
          Returns nil if file cannot be parsed or has no definitions."
         (tree-browser/definitions-nodes
          (tree-browser/file->node-tree path)))

       (defun tree-browser/dir-definitions (dir)
         "Return all elisp function and variable definitions in DIR recursively.
          Returns a list of nodes with :file and :definitions properties, where
          :definitions contains detailed information about each definition."
         (let ((result '()))
           (dolist (file (directory-files-recursively dir "\\.el$"))
             (when-let ((defs (tree-browser/file-definitions file)))
               (setq result (cons (km :file file
                                      :definitions defs)
                                  result))))
           result))

       (pb/comment
        (tree-browser/dir-definitions "/Users/pierrebaille/.doom.d/pb/modes")
        (tree-browser/definitions-nodes (tree-browser/file->node-tree (buffer-file-name))))

       (progn :org

              (defun tree-browser/org-node-tree ()
                "Create a tree structure from the current org-mode buffer.
                 Returns a hierarchical representation suitable for tree-browser."
                (when (eq major-mode 'org-mode)
                  (save-excursion
                    (save-restriction
                      (widen)
                      (goto-char (point-min))
                      (let* ((file-name (when (buffer-file-name)
                                          (file-name-nondirectory (buffer-file-name))))
                             (root-node (km :type "org-file"
                                            :name (or file-name "Org Buffer")
                                            :start (point-min)
                                            :end (point-max)
                                            :children (tree-browser/org-collect-top-level-elements))))
                        root-node)))))

              (defun tree-browser/org-collect-top-level-elements ()
                "Collect top-level elements in the current org buffer.
                 Returns a list of node structures representing the elements."
                (let ((elements '())
                      (top-headlines '())
                      (parsed-buffer (org-element-parse-buffer)))

                  ;; First, find all top-level (level 1) headlines
                  (org-element-map parsed-buffer 'headline
                    (lambda (headline)
                      (when (= (org-element-property :level headline) 1)
                        (push headline top-headlines))))

                  ;; Convert each top-level headline to a node
                  (dolist (headline top-headlines)
                    (push (tree-browser/org-element-to-node headline) elements))

                  ;; Also collect elements outside of any headline (if any)
                  (org-element-map parsed-buffer '(src-block plain-list drawer property-drawer keyword paragraph table)
                    (lambda (element)
                      (when (not (org-element-property :parent element))
                        (push (tree-browser/org-element-to-node element) elements))))

                  (nreverse elements)))

              (defun tree-browser/org-element-to-node (element)
                "Convert an org-element ELEMENT to a node structure for the tree browser."
                (let* ((type (org-element-type element))
                       (begin (org-element-property :begin element))
                       (end (org-element-property :end element))
                       (base-node (km :type (symbol-name type)
                                      :start begin
                                      :end end)))
                  (pcase type
                    ('headline
                     (let* ((title (org-element-property :title element))
                            (title-str (substring-no-properties (org-element-interpret-data title)))
                            (level (org-element-property :level element))
                            (tags (org-element-property :tags element))
                            (children '()))

                       ;; Collect child elements (both headlines and non-headline elements)
                       (dolist (child-type '(property-drawer paragraph src-block plain-list drawer keyword table headline))
                         (org-element-map element child-type
                           (lambda (child)
                             ;; For headlines, only include direct children (next level down)
                             (when (or (and (eq child-type 'headline)
                                            (= (org-element-property :level child) (1+ level))
                                            (not (eq child element)))
                                       ;; For non-headlines, check if this element is a direct child
                                       ;; or is in the section of this headline
                                       (and (not (eq child-type 'headline))
                                            (not (eq child element))
                                            (or
                                             ;; Direct child check
                                             (and (org-element-property :parent child)
                                                  (eq (org-element-property :parent child) element))
                                             ;; Section check - element is in the section of this headline
                                             ;; (section appears right after headline and before sub-headings)
                                             (when-let* ((parent (org-element-property :parent child))
                                                         (grandparent (and parent (org-element-property :parent parent))))
                                               (and (eq (org-element-type parent) 'section)
                                                    (eq grandparent element))))))
                               (push (tree-browser/org-element-to-node child) children)))))

                       (km/put base-node
                               :name (substring-no-properties title-str)
                               :level level
                               :tags tags
                               :children (nreverse children))))

                    ('src-block
                     (let ((language (org-element-property :language element))
                           (parameters (org-element-property :parameters element))
                           (content (org-element-property :value element)))
                       (km/put base-node
                               :name language
                               :language language
                               :parameters parameters
                               :content content)))

                    (_ (let* ((content (org-element-interpret-data element))
                              (ellipse-size 20)
                              (ellipted (> (length content) ellipse-size)))
                         (km/put base-node
                                 :overview (concat (substring-no-properties content 0 (min (length content) ellipse-size))
                                                   (if ellipted "..." ""))
                                 :content (substring-no-properties content)))))))

              (defun tree-browser/org-item-to-node (item)
                "Convert an org list ITEM to a node structure."
                (let* ((begin (org-element-property :begin item))
                       (end (org-element-property :end item))
                       (bullet (org-element-property :bullet item))
                       (checkbox (org-element-property :checkbox item))
                       (tag (org-element-property :tag item))
                       (raw-text (buffer-substring-no-properties begin end))
                       (display-text (if (and tag (not (equal tag "")))
                                         (format "%s :: %s" tag
                                                 (if checkbox
                                                     (pcase checkbox
                                                       ('on "[X]")
                                                       ('off "[ ]")
                                                       ('trans "[-]")
                                                       (_ ""))
                                                   ""))
                                       (substring raw-text 0 (min (length raw-text) 40)))))
                  (km :type "item"
                      :start begin
                      :end end
                      :name display-text
                      :bullet bullet
                      :checkbox checkbox
                      :tag tag))))

       (pb/comment
        (with-current-buffer "pb-prompt.el"
          (tree-browser/node-tree (tree-browser/get-treesit-root)))))

(progn :tree-render

       (progn :window-handling

              (defvar-local tree-browser/window-width 35
                "The width of the tree browser window.")

              (defun tree-browser/fix-window-size ()
                "Fix the size of the tree browser window."
                (when-let ((window (get-buffer-window (current-buffer))))
                  ;; Set window parameters for proper behavior
                  (set-window-parameter window 'no-delete-other-windows t)
                  (set-window-parameter window 'no-other-window nil)
                  (set-window-dedicated-p window t)

                  ;; More reliable way to set window width
                  (let ((width tree-browser/window-width))
                    (unless (= (window-width window) width)
                      ;; First try with window-resize which is more reliable
                      (window-resize window (- width (window-width window)) t)

                      ;; Fallback to adjust-window-trailing-edge if needed
                      (when (not (= (window-width window) width))
                        (adjust-window-trailing-edge window (- width (window-width window)) t))))
                  (window-preserve-size window t t)))

              (defun tree-browser/enforce-window-width (&rest _)
                "Maintain tree browser window width after window configuration changes."
                (when nil
                  (dolist (buffer (buffer-list))
                    (with-current-buffer buffer
                      (when (derived-mode-p 'tree-browser/mode)
                        (when-let ((window (get-buffer-window buffer)))
                          (let ((width tree-browser/window-width))
                            (unless (= (window-width window) width)
                              ;; this seems to only work correctly if the tree-browser is on the extreme left of the frame
                              ;; we have to support other cases
                              (adjust-window-trailing-edge window (- width (window-width window)) t)))))))))

              ;; Add hook for window configuration changes
              (add-hook 'window-configuration-change-hook 'tree-browser/enforce-window-width))

       (defun tree-browser/help ()
         "Display help for tree browser mode, or kill the help buffer if visible."
         (interactive)
         (if-let ((help-buf (get-buffer "*Tree Browser Help*"))
                  (help-win (get-buffer-window help-buf)))
             ;; If help buffer is already visible, close it
             (quit-window nil help-win)
           ;; Otherwise, create and display help
           (let ((help-buffer (get-buffer-create "*Tree Browser Help*")))
             (with-current-buffer help-buffer
               (let ((inhibit-read-only t))
                 (erase-buffer)
                 (insert "Tree Browser Mode Commands\n")
                 (insert "========================\n\n")
                 (insert "Navigation:\n")
                 (insert "  j, down   - Move to next line\n")
                 (insert "  k, up     - Move to previous line\n")
                 (insert "  h         - Decrease tree depth\n")
                 (insert "  l         - Increase tree depth\n")
                 (insert "  u         - Go to parent node\n")
                 (insert "  g g       - Go to beginning of buffer\n")
                 (insert "  G         - Go to end of buffer\n")
                 (insert "  /         - Search forward\n")
                 (insert "  RET       - Go to source and close browser\n")
                 (insert "  p         - Go to source without closing browser\n")
                 (insert "  q         - Quit tree browser\n\n")
                 (insert "View Settings:\n")
                 (insert "  f         - Toggle follow mode (cursor follows tree selection)\n")
                 (insert "  n         - Toggle narrow mode (source narrows to selection)\n")
                 (insert "  c         - Toggle centered mode (center node in source window)\n")
                 (insert "  o         - Place node at top of source window\n")
                 (insert "  s         - Sync tree with current source position\n")
                 (insert "  ?         - Toggle help window\n")
                 (insert "  r         - Manually refresh tree\n\n")
                 (insert "Actions:\n")
                 (insert "  y         - Yank (copy) node content to kill ring\n")
                 (insert "  d         - Close browser and open dired-sidebar\n\n")
                 (insert "Mode status:\n")
                 (insert (format "  Follow mode:   %s\n" (if (bound-and-true-p tree-browser/follow-mode) "On" "Off")))
                 (insert (format "  Narrow mode:   %s\n" (if (bound-and-true-p tree-browser/narrow-mode) "On" "Off")))
                 (insert (format "  Centered mode: %s\n" (if (bound-and-true-p tree-browser/centered-mode) "On" "Off")))
                 (insert "\nPress q to close this help window"))
               (special-mode)
               (local-set-key (kbd "q") 'quit-window))
             ;; Use display-buffer in the source window instead of switch-to-buffer-other-window
             (when-let ((source-buffer (buffer-local-value 'tree-browser/source-buffer (current-buffer)))
                        (source-window (get-buffer-window source-buffer)))
               (with-selected-window source-window
                 (switch-to-buffer help-buffer)))
             ;; Fallback to the regular behavior if no source window found
             (unless (get-buffer-window help-buffer)
               (switch-to-buffer-other-window help-buffer)))))

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
                                          (km/get node :name)
                                          (km/get node :verb)))))
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
                  (let ((type (km/get node-data :type))
                        (is-var-def (km/get node-data :var-def))
                        (is-fun-def (km/get node-data :fun-def))
                        (level (or (km/get node-data :level)
                                   1))
                        (default-face (list :inherit 'default :foreground (doom-darken (doom-color 'fg) 0.1)))
                        (function-face (list :inherit 'default :foreground (doom-darken (doom-color 'violet) 0.15)))
                        (variable-face (list :inherit 'default :foreground (doom-darken (doom-color 'magenta) 0.1)))
                        (expression-face (list :inherit 'default :foreground (doom-darken (doom-color 'teal) 0.2)))
                        (code-face (list :inherit 'default :foreground (doom-color 'violet))))
                    (cond
                     ((string= type "source_file")
                      (insert "  "))
                     ((member type (list "section" "headline"))
                      (insert (propertize "■ " 'face (intern (format "outline-%d" (min level 8))))))
                     (is-var-def
                      (insert (propertize "• " 'face variable-face)))
                     (is-fun-def
                      (insert (propertize "λ " 'face function-face)))
                     ((member type (list "special_form" "list" "function_definition"))
                      (insert (propertize "e " 'face expression-face)))
                     ((string= type "src-block")
                      (insert (propertize "≡ " 'face code-face)))
                     (t
                      (insert (propertize "- " 'face 'font-lock-comment-face))))

                    ;; Insert the name with appropriate face and make it look clickable
                    (insert (propertize (format "%s" name)
                                        'face (cond
                                               ;; Use org-level-N faces for headline types with level info
                                               ((and (member type (list "section" "headline")) level)
                                                (intern (format "outline-%d" (min level 8))))
                                               ((string= type "source_file") 'outline-1)
                                               ((member type (list "section" "headline")) 'font-lock-keyword-face)
                                               ((member type (list "function_definition" "special_form" "list"))
                                                (list :inherit 'default :foreground (doom-darken (doom-color 'fg) 0.1)))
                                               ((string= type "src-block") code-face)
                                               (t 'default))
                                        'mouse-face (list :foreground (doom-color 'fg-alt))
                                        'help-echo "Click to navigate to this node"))

                    (when-let ((second-name (km/get node-data :second-name))
                               (face (list :foreground (doom-darken (doom-blend 'fg 'red 0.8)
                                                                    0.2))))
                      (if (< (length (format "%s %s" name second-name))
                             25)
                          (insert (propertize (concat " " second-name)
                                              face 'face))
                        (progn (insert "\n")
                               (insert prefix)
                               (insert (propertize (if (km/get node-data :package-prefixed?)
                                                       (concat "  /" second-name)
                                                     (concat "  " second-name))
                                                   'face face)))))

                    (insert "\n")

                    ;; Store path and node data as text properties for the whole line
                    (put-text-property start (point) 'tree-path path)
                    (put-text-property start (point) 'node-data node-data)
                    ;; Add clickable property to entire line
                    '(put-text-property start (point) 'mouse-face 'highlight))))

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

       (progn :mode-line

              (defun tree-browser/update-mode-line ()
                "Update the mode line to show active modes."
                (setq mode-line-format
                      (list
                       "%e"
                       mode-line-front-space
                       '(:eval (propertize (buffer-name (buffer-local-value 'tree-browser/source-buffer (current-buffer)))
                                           'face 'font-lock-function-name-face))
                       " ["
                       '(:eval (propertize (if tree-browser/follow-mode "F" "-")
                                           'face (if tree-browser/follow-mode 'font-lock-keyword-face 'shadow)
                                           'help-echo "Follow mode"))
                       '(:eval (propertize (if tree-browser/narrow-mode "N" "-")
                                           'face (if tree-browser/narrow-mode 'font-lock-keyword-face 'shadow)
                                           'help-echo "Narrow mode"))
                       '(:eval (propertize (if tree-browser/centered-mode "C" "-")
                                           'face (if tree-browser/centered-mode 'font-lock-keyword-face 'shadow)
                                           'help-echo "Centered mode"))
                       "] "
                       mode-line-end-spaces))
                (force-mode-line-update))

              ;; Update mode line whenever toggling a mode
              (advice-add 'tree-browser/toggle-follow-mode :after #'tree-browser/update-mode-line)
              (advice-add 'tree-browser/toggle-narrow-mode :after #'tree-browser/update-mode-line)
              (advice-add 'tree-browser/toggle-centered-mode :after #'tree-browser/update-mode-line)
              (advice-add 'tree-browser/toggle-auto-refresh-mode :after #'tree-browser/update-mode-line)

              ;; Add to mode initialization
              (defun tree-browser/initialize-mode-line ()
                "Initialize the mode line format for tree browser."
                (tree-browser/update-mode-line))

              ;; Set up mode line during initialization
              (add-hook 'tree-browser/mode-hook #'tree-browser/initialize-mode-line))

       (progn :utils

              (defun tree-browser/place-node-at-top ()
                "Place the current node at the top of the source buffer window.
                 When centered-mode is enabled, center the node instead."
                (interactive)
                (when-let* ((node-data (get-text-property (line-beginning-position) 'node-data))
                            (start (plist-get node-data :start))
                            (buffer (buffer-local-value 'tree-browser/source-buffer (current-buffer))))
                  (when (buffer-live-p buffer)
                    (let ((window (get-buffer-window buffer t)))
                      (if window
                          (with-selected-window window
                            (goto-char start)
                            (if tree-browser/centered-mode
                                (recenter)
                              (recenter 0))
                            (symex--update-overlay))
                        (with-current-buffer buffer
                          (goto-char start)))
                      (message "Node positioned in window")))))

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

              (defun tree-browser/get-node-depth (node current-pos depth)
                "Calculate the depth of the node containing CURRENT-POS in the tree starting from NODE.
                 Returns the depth as a number, or nil if the position is not in this subtree."
                (when (and node (km? node))
                  (let ((start (km/get node :start))
                        (end (km/get node :end))
                        (children (km/get node :children)))

                    ;; Check if current position is in this node's range
                    (when (and start end (>= current-pos start) (<= current-pos end))
                      ;; Check children first to find the deepest containing node
                      (when children
                        (cl-loop for child in children
                                 for child-depth = (tree-browser/get-node-depth child current-pos (1+ depth))
                                 when child-depth return child-depth
                                 finally return depth))
                      ;; If no child contains the position or no children, return current depth
                      depth)))))

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
             (tree-browser/mode)
             (setq-local tree-browser/data tree)
             (setq-local tree-browser/source-buffer (or source-buffer (current-buffer)))
             (when (not buf-exist)
               (setq-local tree-browser/max-depth 1))
             (tree-browser/render tree)
             (goto-char (point-min)))

           ;; Only create a new window if one doesn't exist for this buffer
           (unless existing-window
             ;; Create window with exact width from the beginning
             (let ((window (split-window (selected-window) tree-browser/window-width 'left t)))
               (select-window window)
               (switch-to-buffer buf)
               (set-window-parameter window 'no-delete-other-windows t)
               (set-window-dedicated-p window t)))

           ;; If window already exists, just switch to it
           (when existing-window
             (select-window existing-window))

           ;; Make sure the width is correct
           (tree-browser/fix-window-size)

           ;; Position cursor at the node containing the current position in source buffer
           (tree-browser/position-cursor-at-node current-pos)

           buf))

       (defun tree-browser/org-navigate-buffer ()
         "Create and display a tree browser for the current org buffer structure."
         (interactive)
         (when (eq major-mode 'org-mode)
           (let* ((tree (tree-browser/org-node-tree))
                  (current-pos (point))
                  (browser-buffer (tree-browser/create
                                   tree
                                   (format "*Org Tree: %s*" (buffer-name))
                                   (current-buffer))))
             (with-current-buffer browser-buffer
               (setq-local tree-browser/window-width 45)
               (setq-local tree-browser/max-depth 3) ;; Show more depth by default for org
               (tree-browser/refresh)
               (tree-browser/fix-window-size)
               (tree-browser/position-cursor-at-node current-pos)))))

       (defun tree-browser/elisp-navigate-buffer ()
         "Create a tree browser for elisp/treesitter-supported buffers.
          Analyzes the buffer using treesitter, sets an appropriate depth,
          and positions the cursor at the current node."
         (when-let ((root (tree-browser/get-treesit-root)))
           (let* ((tree (tree-browser/node-tree root))
                  (current-pos (point))
                  ;; Find the depth of the node at current position
                  (node-depth (or (tree-browser/get-node-depth tree current-pos 0) 1))
                  ;; Add some context by showing a level or two above
                  (display-depth (+ node-depth 2)))

             ;; Create tree browser with the calculated depth
             (let ((browser-buffer (tree-browser/create
                                    tree
                                    (format "*Tree Browser: %s*" (buffer-name))
                                    (current-buffer))))

               ;; Set the max depth to show the node and some context
               (with-current-buffer browser-buffer
                 (setq-local tree-browser/max-depth display-depth)
                 ;; Refresh to apply the new depth
                 (tree-browser/refresh)
                 (tree-browser/fix-window-size)
                 ;; Position cursor at the node containing the current position
                 (tree-browser/position-cursor-at-node current-pos))))))

       (defun tree-browser/navigate-buffer ()
         "Analyze current buffer and display as tree browser.
          Handles different major modes appropriately:
          - org-mode: Uses org-specific tree building
          - Other modes: Uses treesitter-based parsing when available"
         (interactive)
         (cond
          ;; For org-mode buffers
          ((eq major-mode 'org-mode)
           (tree-browser/org-navigate-buffer))

          ;; For treesitter-supported buffers
          ((tree-browser/get-treesit-root)
           (tree-browser/elisp-navigate-buffer))

          ;; Fallback for unsupported buffers
          (t
           (message "Tree browser is not supported for this buffer type"))))

       (defun tree-browser/navigate-and-search-buffer ()
         "Navigate to the tree browser for the current buffer and immediately start live search."
         (interactive)
         (tree-browser/navigate-buffer)
         ;; Wait for the tree browser to be created and displayed
         (run-with-idle-timer 0.1 nil
                              (lambda ()
                                (when (derived-mode-p 'tree-browser/mode)
                                  (tree-browser/live-search))))))

(progn :bindings
       (when (featurep 'evil)
         (evil-define-key 'normal tree-browser/mode-map
           (kbd "j") 'tree-browser/next-line
           (kbd "k") 'tree-browser/prev-line
           (kbd "h") 'tree-browser/decrease-depth
           (kbd "l") 'tree-browser/increase-depth
           (kbd "y") 'tree-browser/yank-node
           (kbd "o") 'tree-browser/insert-after-node
           (kbd "x") 'tree-browser/delete-node
           (kbd "q") 'tree-browser/quit
           (kbd "d") 'tree-browser/open-dired-sidebar
           (kbd "C-h") 'tree-browser/open-dired-sidebar
           (kbd "RET")  (lambda () (interactive) (tree-browser/goto-source t))
           (kbd "r") 'tree-browser/refresh
           (kbd "n") 'tree-browser/toggle-narrow-mode
           (kbd "f") 'tree-browser/toggle-follow-mode
           (kbd "c") 'tree-browser/toggle-centered-mode
           (kbd "g g") 'beginning-of-buffer
           (kbd "G") 'end-of-buffer
           (kbd "s-q") 'tree-browser/query
           (kbd "/") 'tree-browser/live-search)))

(provide 'tree-browser)
