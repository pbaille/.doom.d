;;; pb/xp/modes/tree-browser.el -*- lexical-binding: t; -*-

(require 'evil)

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
           (let ((centered tree-browser/centered-mode))
             (when (buffer-live-p buffer)
               (let ((window (get-buffer-window buffer t)))
                 (if window
                     (with-selected-window window
                       (goto-char start)
                       ;; Always call evil-scroll-line-to-top first to ensure proper positioning
                       (evil-scroll-line-to-top nil)
                       ;; Apply centering only if centered mode is enabled
                       (when centered (print "centered")(recenter))
                       (symex--update-overlay))
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
           (quit-window)
           (kill-buffer buf)
           (widen)
           (balance-windows)))

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
                 (recenter)))
             (balance-windows))))) ;; Prevent text selection


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

(progn :depth

       (defun tree-browser/increase-depth ()
         "Increase the maximum depth of the tree display."
         (interactive)
         (let ((current-path (get-text-property (line-beginning-position) 'tree-path)))
           (setq-local tree-browser/max-depth (1+ tree-browser/max-depth))
           (tree-browser/refresh)
           (when current-path
             (tree-browser/goto-path current-path))
           (back-to-indentation)))

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
                 (if (and file-name
                          (> (length name) (length file-name))
                          (string-prefix-p file-name name))
                     (substring name (1+ (length file-name)))
                   name)
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
                            :name (file-name-nondirectory (buffer-file-name))
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
                           (verb-node (treesit-node-child node 1))
                           (verb (when verb-node (intern (treesit-node-text verb-node t))))
                           (name (when (string= "symbol" (treesit-node-type name-node))
                                   (treesit-node-text name-node t)))
                           (is-var-def (member verb '(defvar defvar-local)))
                           (is-fun-def (eq verb 'defun)))
                      (km/put base
                              :verb verb
                              :name name
                              :short-name (tree-browser/remove-package-prefix name)
                              :var-def is-var-def
                              :fun-def is-fun-def)))
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
                 (insert "  a         - Toggle auto-refresh mode (refresh on save)\n")
                 (insert "  r         - Manually refresh tree\n\n")
                 (insert "Mode status:\n")
                 (insert (format "  Follow mode:   %s\n" (if (bound-and-true-p tree-browser/follow-mode) "On" "Off")))
                 (insert (format "  Narrow mode:   %s\n" (if (bound-and-true-p tree-browser/narrow-mode) "On" "Off")))
                 (insert (format "  Centered mode: %s\n" (if (bound-and-true-p tree-browser/centered-mode) "On" "Off")))
                 (insert (format "  Auto-refresh:  %s\n" (if (bound-and-true-p tree-browser/auto-refresh-mode) "On" "Off")))
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
                  (let ((type (km/get node-data :type))
                        (is-var-def (km/get node-data :var-def))
                        (is-fun-def (km/get node-data :fun-def)))
                    (cond
                     ((string= type "source_file")
                      (insert "  "))
                     ((string= type "section")
                      (insert (propertize "■ " 'face 'font-lock-keyword-face)))
                     (is-var-def
                      (insert (propertize "• " 'face 'font-lock-variable-name-face)))
                     (is-fun-def
                      (insert (propertize "λ " 'face 'font-lock-function-name-face)))
                     ((member type (list "special_form" "list" "function_definition"))
                      (insert (propertize "• " 'face 'font-lock-doc-face)))
                     (t
                      (insert (propertize "- " 'face 'font-lock-comment-face))))

                    ;; Insert the name with appropriate face and make it look clickable
                    (insert (propertize (format "%s" name)
                                        'face (cond
                                               ((string= type "source_file") 'font-lock-constant-face)
                                               ((string= type "section") 'font-lock-keyword-face)
                                               ((member type (list "function_definition" "special_form" "list"))
                                                'font-lock-comment-face)
                                               (t 'font-lock-comment-face))
                                        'mouse-face 'highlight
                                        'help-echo "Click to navigate to this node"))

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
                       '(:eval (propertize (if tree-browser/auto-refresh-mode "A" "-")
                                           'face (if tree-browser/auto-refresh-mode 'font-lock-keyword-face 'shadow)
                                           'help-echo "Auto-refresh mode"))
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
         "Analyze current buffer with treesitter and display as tree.
          Sets the tree's max depth based on the node at point's depth and positions the cursor at that node."
         (interactive)
         (when-let ((root (tree-browser/get-treesit-root)))
           (let* ((tree (tree-browser/node-tree root))
                  (current-pos (point))
                  ;; Find the depth of the node at current position
                  (node-depth (or (tree-browser/get-node-depth tree current-pos 0) 1))
                  ;; Add some context by showing a level or two above
                  (display-depth (+ node-depth 2)))

             ;; Create tree browser with the calculated depth
             (let ((browser-buffer (tree-browser/create tree
                                                        (format "*Tree Browser: %s*" (buffer-name))
                                                        (current-buffer))))

               ;; Set the max depth to show the node and some context
               (with-current-buffer browser-buffer
                 (setq-local tree-browser/max-depth display-depth)
                 ;; Refresh to apply the new depth
                 (tree-browser/refresh)
                 ;; Position cursor at the node containing the current position
                 (tree-browser/position-cursor-at-node current-pos)))))))

(progn :bindings
       (when (featurep 'evil)
         (evil-define-key 'normal tree-browser/mode-map
           (kbd "j") 'tree-browser/next-line
           (kbd "k") 'tree-browser/prev-line
           (kbd "h") 'tree-browser/decrease-depth
           (kbd "l") 'tree-browser/increase-depth
           (kbd "y") 'tree-browser/yank-node
           (kbd "q") 'tree-browser/quit
           (kbd "RET")  (lambda () (interactive) (tree-browser/goto-source t))
           (kbd "r") 'tree-browser/refresh
           (kbd "n") 'tree-browser/toggle-narrow-mode
           (kbd "f") 'tree-browser/toggle-follow-mode
           (kbd "c") 'tree-browser/toggle-centered-mode
           (kbd "g g") 'beginning-of-buffer
           (kbd "G") 'end-of-buffer
           (kbd "/") 'isearch-forward)))

(provide 'tree-browser)
