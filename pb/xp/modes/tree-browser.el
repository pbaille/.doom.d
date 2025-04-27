;;; pb/xp/modes/tree-browser.el -*- lexical-binding: t; -*-

;; Define the maximum depth visible in the browser when first created
(defvar-local tree-browser/max-depth 0
  "Maximum depth of the tree to display initially.")

(defvar tree-browser/mode-map
  (make-sparse-keymap)
  "Keymap for `tree-browser/mode'.")

(define-derived-mode tree-browser/mode special-mode "Tree Browser"
  "Major mode for browsing nested km structures.
   \\{tree-browser/mode-map}"
  (setq buffer-read-only t)
  (setq-local truncate-lines t))

(progn :actions

       (defun tree-browser/next-line ()
         "Move to next line in tree browser."
         (interactive)
         (forward-line 1))

       (defun tree-browser/prev-line ()
         "Move to previous line in tree browser."
         (interactive)
         (forward-line -1))

       (defun tree-browser/quit ()
         "Quit the tree browser."
         (interactive)
         (quit-window)
         '(kill-buffer))

       (defun tree-browser/goto-source ()
         "Go to the source location of the node at point."
         (interactive)
         (when-let* ((node-data (get-text-property (line-beginning-position) 'node-data))
                     (start (plist-get node-data :start))
                     (buffer (buffer-local-value 'tree-browser/source-buffer (current-buffer))))
           (when (buffer-live-p buffer)
             (pop-to-buffer buffer)
             (goto-char start)
             (recenter)))))

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

       (defun tree-browser/node-tree (node)
         (when node

           (let* ((type (treesit-node-type node))
                  (start (treesit-node-start node))
                  (end (treesit-node-end node))
                  (base (km :type type :start start :end end)))
             (cond ((string= "comment" type) nil)
                   ((string= "source_file" type)
                    (km/put base :children (seq-keep #'tree-browser/node-tree
                                                     (treesit-node-children node))))
                   ((string= "special_form" type)
                    (if (string= "progn" (treesit-node-type (treesit-node-child node 1)))
                        (km/put base
                                :type "section"
                                :name (intern (treesit-node-text (treesit-node-child node 2) t))
                                :children (seq-keep #'tree-browser/node-tree
                                                    (sq/butlast
                                                     (sq/drop (treesit-node-children node)
                                                              3))))
                      (km/put base
                              :kind (intern (treesit-node-type (treesit-node-child node 1))))))
                   ((string= "list" type)
                    (km/put base
                            :verb (intern (treesit-node-text (treesit-node-child node 1) t))))
                   (t base)))))

       (pb/comment
        (with-current-buffer "pb-prompt.el"
          (tree-browser/node-tree (tree-browser/get-treesit-root)))))

(progn :tree-render

       (defun tree-browser/render (tree)
         "Render TREE in the current buffer using tree structure produced by node-tree."
         (let ((inhibit-read-only t))
           (erase-buffer)
           (tree-browser/render-node tree 0 () 0)))

       (defun tree-browser/render-node (node depth path current-depth)
         "Render NODE at DEPTH with PATH prefix as the key path at CURRENT-DEPTH level.
          This function is designed to work with tree structures produced by tree-browser/node-tree."
         (when (and (km? node) (<= current-depth tree-browser/max-depth))
           (let* ((type (plist-get node :type))
                  (name (plist-get node :name))
                  (start (plist-get node :start))
                  (end (plist-get node :end))
                  (children (plist-get node :children))
                  (disp-name (or name type)))

             ;; Insert the current node
             (tree-browser/insert-line disp-name depth path (and children (not (null children)))
                                       (km :type type :name name :start start :end end))

             ;; Process children if any and if we're not at max depth
             (when (and children (< current-depth tree-browser/max-depth))
               (dolist (child children)
                 (let* ((child-name (or (plist-get child :name)
                                        (plist-get child :type)))
                        (child-path (cons (pb/string child-name) path)))
                   (tree-browser/render-node child (+ depth 2) child-path (1+ current-depth))))))))

       (defun tree-browser/insert-line (disp-name depth path has-children node-data)
         "Insert a line for DISP-NAME at DEPTH with PATH.
          Indicate if it HAS-CHILDREN and store NODE-DATA as properties."
         (let ((start (point))
               (prefix (make-string depth ? )))
           (insert prefix)

           ;; Display different symbols based on node type
           (let ((type (plist-get node-data :type)))
             (cond
              ((string= type "section")
               (insert (propertize "§ " 'face 'font-lock-doc-face)))
              ((string= type "special_form")
               (insert (propertize "λ " 'face 'font-lock-keyword-face)))
              ((string= type "list")
               (insert (propertize "• " 'face 'font-lock-function-name-face)))
              (has-children
               (insert (propertize "+ " 'face 'font-lock-comment-face)))
              (t
               (insert (propertize "- " 'face 'font-lock-variable-name-face)))))

           ;; Insert the name with appropriate face
           (insert (propertize (format "%s" disp-name)
                               'face (cond
                                      ((plist-get node-data :name) 'font-lock-constant-face)
                                      (t 'font-lock-type-face))))

           ;; Display location info if available
           (when-let ((start-pos (plist-get node-data :start))
                      (end-pos (plist-get node-data :end)))
             (insert (propertize (format " (%d-%d)" start-pos end-pos)
                                 'face 'font-lock-comment-face)))

           (insert "\n")

           ;; Store path and node data as text properties
           (put-text-property start (point) 'tree-path path)
           (put-text-property start (point) 'node-data node-data)))

       (defun tree-browser/create (tree &optional buffer-name source-buffer)
         "Create a browser for TREE in a new buffer named BUFFER-NAME or *Tree Browser*.
          SOURCE-BUFFER is the original buffer that the tree represents."
         (let ((buf (get-buffer-create (or buffer-name "*Tree Browser*"))))
           (with-current-buffer buf
             (tree-browser/mode)
             (setq-local tree-browser/data tree)
             (setq-local tree-browser/max-depth 3) ;; Default max depth
             (setq-local tree-browser/source-buffer (or source-buffer (current-buffer)))
             (tree-browser/render tree)
             (goto-char (point-min)))
           (switch-to-buffer-other-window buf)
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

(when (featurep 'evil)
  (evil-define-key 'normal tree-browser/mode-map
    (kbd "j") 'tree-browser/next-line
    (kbd "k") 'tree-browser/prev-line
    (kbd "q") 'tree-browser/quit
    (kbd "RET") 'tree-browser/goto-source
    (kbd ">") 'tree-browser/increase-depth
    (kbd "<") 'tree-browser/decrease-depth
    (kbd "r") 'tree-browser/refresh
    (kbd "g g") 'beginning-of-buffer
    (kbd "G") 'end-of-buffer
    (kbd "/") 'isearch-forward))
