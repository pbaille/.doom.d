;;; pb-lisp.el --- Lisp evil mode inspired by Symex -*- lexical-binding: t; -*-

;; Author: Pierre Baille
;; URL: https://github.com/pbaille
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Lisp evil mode inspired by Symex.

;;; Code:

(require 'sq)
(require 'evil)
(require 'treesit)
(require 'pb-elisp)
(require 'doom-themes)
(require 'pb-color)

(progn :mode-definition

       (evil-define-state pb-lisp
         "PB Lisp state."
         :tag " PB/LISP "
         :message "-- PB/LISP --"
         :enable (normal)
         :entry-hook (pb-lisp/enter-mode)
         :exit-hook (pb-lisp/exit-mode))

       (setq evil-pb-lisp-state-cursor `(box "magenta"))
       (progn (setq pb-lisp/overlay-background-color "#352B3B")
              (defface pb-lisp/overlay-face
                `((t
                   :inherit symex--current-node-face
                   :extend nil :background ,pb-lisp/overlay-background-color))
                "Face used to highlight the current tree node."
                :group 'pb-lisp/faces))

       (defvar pb-lisp/modes
         '(org-mode clojure-mode clojurescript-mode clojurec-mode
           emacs-lisp-mode fennel-mode sheme-mode racket-mode))

       (defvar pb-lisp/major-mode->treesit-lang
         '((emacs-lisp-mode . elisp)
           (clojure-mode . clojure)
           (clojurescript-mode . clojure)
           (clojurec-mode . clojure)
           (org-mode . org)))

       (defun pb-lisp/parser-setup ()
         "Setup tree-sitter parser for current elisp buffer."
         (if (eq major-mode 'org-mode)
             (pb-org-babel/focus-code-block)
           (when-let ((lang (or (treesit-language-at (point))
                                (alist-get major-mode pb-lisp/major-mode->treesit-lang))))
             (print (cons "parser setup " lang))
             (when (treesit-language-available-p lang)
               (treesit-parser-create lang)))))

       (defun pb-lisp/enter-mode ()
         "Run when on entering sorg mode."
         (when (member major-mode pb-lisp/modes)
           (setq-local pb-lisp/current-node nil)
           (pb-lisp/parser-setup)
           (hl-line-mode -1)
           (setq-local pb-lisp/selection-size 1)
           (goto-char (car (pb-lisp/get-current-node-bounds)))
           (pb-lisp/update-overlay)
           (pb-lisp/set-local-fringe-face)))

       (defun pb-lisp/exit-mode ()
         "Run on exiting sorg mode.
          oiu"
         (print "exit pb-lisp")
         (pb-lisp/delete-overlay)
         (setq-local header-line-format nil)
         (when (eq major-mode 'org-mode)
           (pb-org-babel/init-buffer))
         (pb-lisp/reset-local-fringe-face)))

(progn :selection

       (defvar-local pb-lisp/selection-size 1)
       (quote
        (setq-local pb-lisp/selection-size 1))

       (defun pb-lisp/extend-selection ()
         (setq-local pb-lisp/selection-size (1+ pb-lisp/selection-size))
         (pb-lisp/update-overlay))

       (defun pb-lisp/shrink-selection ()
         (setq-local pb-lisp/selection-size (max 1 (1- pb-lisp/selection-size)))
         (pb-lisp/update-overlay))

       (defun pb-lisp/reset-selection ()
         (setq-local pb-lisp/selection-size 1)
         (pb-lisp/update-overlay))

       (defun pb-lisp/selection-bounds ()
         (let ((nodes (pb-lisp/get-selected-nodes)))
           (cons (treesit-node-start (car nodes))
                 (treesit-node-end (sq/last nodes)))))

       (defun pb-lisp/selection-start ()
         (car (pb-lisp/selection-bounds)))

       (defun pb-lisp/selection-end ()
         (cdr (pb-lisp/selection-bounds)))

       (defun pb-lisp/set-selection (bounds)
         "Set selection to all named nodes within BOUNDS.
          BOUNDS is a cons cell (start . end) with buffer positions."
         (pb/let [(cons start end) bounds
                  parser-lang (alist-get major-mode pb-lisp/major-mode->treesit-lang)]
           (when parser-lang
             (let* ((first-node (treesit-node-at start parser-lang))
                    (first-node (pb-lisp/get-topmost-node first-node))
                    (node-start (treesit-node-start first-node))
                    (count 0)
                    (current-node first-node))
               ;; Set current-node to the first node in the selection
               (setq-local pb-lisp/current-node first-node)
               ;; Calculate how many siblings are in the selection
               (while (and current-node
                           (<= (treesit-node-end current-node) end))
                 (setq current-node (treesit-node-next-sibling current-node t))
                 (when current-node (setq count (1+ count))))

               ;; Update selection size
               (setq-local pb-lisp/selection-size (max 1 count))

               ;; Go to start of selection and update overlay
               (goto-char node-start)
               (pb-lisp/update-overlay)))))

       (progn :overlay

              (defface pb-lisp/overlay-face
                `((t
                   :inherit symex--current-node-face
                   :extend nil :background ,pb-lisp/overlay-background-color))
                "Face used to highlight the current tree node."
                :group 'pb-lisp/faces)

              (defvar pb-lisp/overlays ()
                "List of active overlays used to highlight Lisp nodes.")

              (defun pb-lisp/delete-overlay ()
                "Delete all highlight overlays from the current buffer."
                (mapc #'delete-overlay pb-lisp/overlays)
                (setq pb-lisp/overlays nil))

              (defun pb-lisp/update-overlay ()
                "Update the highlight overlay to match the start/end position of the selected nodes."
                (interactive)
                (pb-lisp/delete-overlay)
                (let* ((bounds (pb-lisp/selection-bounds))
                       (start (car bounds))
                       (end (cdr bounds))
                       (column (current-column)))
                  (save-excursion
                    (goto-char start)
                    (let* ((end-of-line (save-excursion (goto-char (line-end-position)) (point)))
                           (main-overlay (make-overlay start (min end end-of-line))))
                      ;; Add the main overlay for the entire selection, but skip the indentation
                      (overlay-put main-overlay 'face 'pb-lisp/overlay-face)
                      (push main-overlay pb-lisp/overlays)
                      ;;
                      ;; Create separate indentation overlays for each line except the first
                      (when (> (count-lines start end) 1)
                        (forward-line 1)
                        ;; Process each line except the first
                        (while (< (point) end)
                          (let* ((line-start (+ column (line-beginning-position)))
                                 (line-end (min (line-end-position) end))
                                 (line-empty (looking-at-p "[ \t]*$"))
                                 (line-overlay (when (and (not line-empty) (< line-start line-end))
                                                 (make-overlay line-start line-end))))
                            ;; Only add overlay for non-empty parts after indentation
                            (when line-overlay
                              (overlay-put line-overlay 'face 'pb-lisp/overlay-face)
                              (push line-overlay pb-lisp/overlays)))
                          (forward-line 1)))))))))

(progn :fringe

       (defun pb-lisp/set-local-fringe-face ()
         (face-remap-add-relative 'fringe :background pb-lisp/overlay-background-color)
         (flycheck-refresh-fringes-and-margins))

       (defun pb-lisp/reset-local-fringe-face ()
         (face-remap-add-relative 'fringe :background (face-attribute 'default :background))
         (flycheck-refresh-fringes-and-margins))

       (pb/comment (pb-lisp/reset-local-fringe-face)
                   (pb-lisp/set-local-fringe-face)))

(progn :current-node

       (defvar-local pb-lisp/current-node nil)

       (defun pb-lisp/node-depth (node)
         "Return the depth of NODE in its tree."
         (let ((depth 0)
               (current node))
           (while (treesit-node-parent current)
             (setq current (treesit-node-parent current))
             (cl-incf depth))
           depth))

       (defun pb-lisp/get-topmost-node (node)
         "Return the highest node in the tree starting from NODE.
          
          The returned node is the highest possible node that has the same
          start position as NODE, but excludes the root source_file node."
         (let ((node-start-pos (treesit-node-start node))
               (parent (treesit-node-parent node)))
           (if parent
               (let ((parent-pos (treesit-node-start parent))
                     (parent-type (treesit-node-type parent)))
                 (if (and (eq node-start-pos parent-pos)
                          (not (member parent-type '("source" "source_file"))))
                     (pb-lisp/get-topmost-node parent)
                   node))
             node)))

       (defun pb-lisp/stacked-nodes (node)
         (let ((node-start-pos (treesit-node-start node))
               (parent (treesit-node-parent node)))
           (if parent
               (let ((parent-pos (treesit-node-start parent))
                     (parent-type (treesit-node-type parent)))
                 (if (and (eq node-start-pos parent-pos)
                          (not (member parent-type '("source" "source_file"))))
                     (cons node (pb-lisp/stacked-nodes parent))
                   (list node)))
             (list node))))

       (defun pb-lisp/get-current-node ()
         "Get the tree-sitter node at point."
         (if (treesit-parser-list)
             (if (or (not pb-lisp/current-node)
                     (treesit-node-check pb-lisp/current-node 'outdated))
                 (pb-lisp/get-topmost-node
                  (treesit-node-at (point)
                                   (or (treesit-language-at (point))
                                       (alist-get major-mode pb-lisp/major-mode->treesit-lang))))
               pb-lisp/current-node)
           (message "tree-sit not enabled")))

       (defun pb-lisp/get-selected-nodes ()
         "Get a list of selected nodes from the current selection.
          Returns a list with the first node and subsequent siblings based on selection size."
         (let* ((first-node (pb-lisp/get-current-node))
                (result (list first-node)))
           (when (> pb-lisp/selection-size 1)
             (let ((sibling first-node)
                   (n (1- pb-lisp/selection-size)))
               (dotimes (_ n)
                 (setq sibling (treesit-node-next-sibling sibling t))
                 (when sibling
                   (push sibling result)))))
           (nreverse result)))

       (defun pb-lisp/get-current-nodes ()
         "Get the tree-sitter nodes at point."
         (if (treesit-parser-list)
             (pb-lisp/stacked-nodes (treesit-node-at (point)
                                                     (alist-get major-mode pb-lisp/major-mode->treesit-lang)))
           (message "tree-sit not enabled")))

       (defun pb-lisp/get-current-node-bounds ()
         "Get the start and end positions of the current tree-sitter node.
          Returns a cons cell (start . end) with buffer positions."
         (let ((node (pb-lisp/get-current-node)))
           (cons (treesit-node-start node) (treesit-node-end node))))

       (defun pb-lisp/current-node-as-string ()
         "Get the string content of the current treesit node.
          Returns the text between the start and end positions of the current
          node as determined by `pb-lisp/get-current-node', or nil if no valid
          node is found at point."
         (let* ((node (pb-lisp/get-current-node))
                (start (treesit-node-start node))
                (end (treesit-node-end node)))
           (when (and start end)
             (buffer-substring-no-properties start end))))

       (defun pb-lisp/node-infos (&optional node)
         (interactive)
         (let* ((node (or node (pb-lisp/get-current-node)))
                (parent (treesit-node-parent node)))
           (km :node-type (treesit-node-type node)
               :idx (treesit-node-index node)
               :field-name (treesit-node-field-name node)
               :parent-node (treesit-node-parent node)
               :siblings-count (when parent (treesit-node-child-count parent))
               :children (mapcar (lambda (child)
                                   (km :children-count (treesit-node-child-count child)
                                       :field-name (treesit-node-field-name child)
                                       :node-type (treesit-node-type child)
                                       :idx (treesit-node-index child)))
                                 (treesit-node-children node)))))

       (defun pb-lisp/log-node (&optional node)
         (interactive)
         (pb-elisp/display-expression
          (pb-lisp/node-infos node)
          #'km/pp))

       (defun pb-lisp/log-nodes (&optional node)
         (interactive)
         (pb-elisp/display-expression
          (mapcar #'pb-lisp/node-infos
                  (pb-lisp/get-current-nodes))
          #'km/pp))

       (defun pb-lisp/current-selection-as-string ()
         "Get the string content of the current selection overlay.
          Returns the text within the bounds of `pb-lisp/current-overlay', or
          nil if the overlay is not properly set or has invalid bounds."
         (let* ((start (pb-lisp/selection-start))
                (end (pb-lisp/selection-end)))
           (when (and start end)
             (buffer-substring-no-properties start end)))))

(progn :motion

       (defvar-local pb-lisp/escape-top-level-function nil
         "Function to call when attempting to navigate up from a top-level node.
          When set, this function will be called instead of displaying a message
          when the user tries to navigate to the parent of a top-level node.
          This allows for custom behavior when reaching the boundary of the
          current syntax tree, such as exiting the current mode or moving focus
          to a different area.")

       (defvar-local pb-lisp/enter-node-function nil
         "Custom function to call when attempting to enter a node.
          When set, this function will be called instead of the default behavior
          when using `pb-lisp/goto-first-child`. This allows for custom handling
          of node traversal, such as specialized behavior for certain node types
          or context-dependent navigation logic.")

       (defvar pb-lisp/skipped-node-types
         '("\n" ")" "]" "}"))

       (defun pb-lisp/goto-node (node message)
         "Go to the start position of NODE or display MESSAGE if node is nil."
         (if node
             (progn (goto-char (treesit-node-start node))
                    (setq-local pb-lisp/current-node node)
                    (pb-lisp/update-overlay)
                    node)
           (progn (message message)
                  nil)))

       (defun pb-lisp/goto-parent ()
         "Move to the beginning of the parent node."
         (interactive)
         (let* ((node (pb-lisp/get-current-node))
                (parent (treesit-node-parent node)))
           (pb-lisp/reset-selection)
           (if parent
               (pb-lisp/goto-node parent "No parent node found")
             (if pb-lisp/escape-top-level-function
                 (funcall pb-lisp/escape-top-level-function)
               (message "Top level node")))))

       (defun pb-lisp/get-node-child-index (node parent)
         "Get the index of NODE among the named children of PARENT.
          Returns nil if NODE is not a child of PARENT."
         (when (and node parent)
           (let ((child-count (treesit-node-child-count parent t)))
             (cl-loop for i from 0 below child-count
                      for child = (treesit-node-child parent i t)
                      when (equal node child)
                      return i))))

       (defun pb-lisp/get-node-idx (node)
         "Get the index of NODE among the named children of PARENT.
          Returns nil if NODE is not a child of PARENT."
         (when node
           (let* ((parent (treesit-node-parent node))
                  (children (treesit-node-children parent)))
             (cl-loop for c in children
                      for i from 0
                      when (eq node c)
                      return i))))

       (defun pb-lisp/goto-first-child ()
         "Move to the first child of current node."
         (interactive)
         (let* ((node (pb-lisp/get-current-node))
                (child (and node (treesit-node-child node 0 t))))
           (pb-lisp/goto-node (or child node) "No child node found")))

       (defun pb-lisp/enter-node ()
         "Move to the first child of current node."
         (interactive)
         (if (> pb-lisp/selection-size 1)
             (pb-lisp/reset-selection)
           (if pb-lisp/enter-node-function
               (funcall pb-lisp/enter-node-function)
             (pb-lisp/goto-first-child))))

       (defun pb-lisp/goto-nth-child (idx)
         (let* ((node (pb-lisp/get-current-node))
                (child (treesit-node-child node idx t)))
           (when child
             (pb-lisp/goto-node child "Child not found"))))

       (defun pb-lisp/goto-next-sibling_simple ()
         "Move to the next sibling node."
         (interactive)
         (let* ((node (pb-lisp/get-current-node))
                (next-sibling (treesit-node-next-sibling node t)))
           (if next-sibling
               (pb-lisp/goto-node next-sibling "No next sibling found"))))

       (defun pb-lisp/goto-next-sibling_less-simple ()
         "Move to the next sibling node."
         (interactive)
         (let* ((node (pb-lisp/get-current-node))
                (parent (treesit-node-parent node))
                (next-node (treesit-node-child parent (1+ (treesit-node-index node)))))
           (pb-lisp/goto-node (when (not (member (treesit-node-type next-node)
                                                 '(")" "]" "}")))
                                next-node)
                              "No next sibling found")))

       (defun pb-lisp/goto-next-sibling ()
         "Move to the next sibling node.
          Skips over closing delimiters and continues to the next valid node."
         (interactive)
         (let* ((node (pb-lisp/get-current-node))
                (parent (treesit-node-parent node))
                (next-sibling nil))
           (when parent
             (let* ((idx (treesit-node-index node))
                    (child-count (treesit-node-child-count parent))
                    (next-idx (1+ idx)))
               ;; Skip over closing delimiters until we find a valid node or reach the end
               (while (and (< next-idx child-count)
                           (not next-sibling))
                 (let ((candidate (treesit-node-child parent next-idx)))
                   (if (member (treesit-node-type candidate) pb-lisp/skipped-node-types)
                       (setq next-idx (1+ next-idx))
                     (setq next-sibling candidate))))))
           (pb-lisp/goto-node next-sibling "No next sibling found")))

       (defun pb-lisp/goto-prev-sibling ()
         "Move to the previous sibling node."
         (interactive)
         (let* ((node (pb-lisp/get-current-node))
                (prev-sibling (treesit-node-prev-sibling node t)))
           (pb-lisp/goto-node prev-sibling "No previous sibling found")))

       (defun pb-lisp/goto-first-sibling ()
         "Move to the first sibling node (first child of parent)."
         (interactive)
         (let* ((node (pb-lisp/get-current-node))
                (parent (treesit-node-parent node))
                (first-sibling (and parent (treesit-node-child parent 0 t))))
           (pb-lisp/goto-node first-sibling "No first sibling found")))

       (defun pb-lisp/goto-last-sibling ()
         "Move to the last sibling node (last child of parent)."
         (interactive)
         (let* ((node (pb-lisp/get-current-node))
                (parent (treesit-node-parent node))
                (child-count (and parent (treesit-node-child-count parent t)))
                (last-sibling (and child-count (> child-count 0)
                                   (treesit-node-child parent (1- child-count) t))))
           (pb-lisp/goto-node last-sibling "No last sibling found")))

       (defun pb-lisp/goto-last-child ()
         "Move to the last child of current node."
         (interactive)
         (let* ((node (pb-lisp/get-current-node))
                (count (treesit-node-child-count node))
                (child (and (> count 0) (treesit-node-child node (1- count)))))
           (pb-lisp/goto-node child "No child node found")))

       (defun pb-lisp/goto-next-line ()
         "Move cursor to the next line, disabling overlay and resetting current-node."
         (interactive)
         (let ((current-pos (point)))
           (forward-line 1)
           (when (eq (point) current-pos) ; If we're at the end of the buffer
             (goto-char (point-max)))
           (setq-local pb-lisp/current-node nil) ; Reset current node
           (pb-lisp/delete-overlay)              ; Remove highlighting
           (back-to-indentation)))

       (defun pb-lisp/goto-previous-line ()
         "Move cursor to the previous line, disabling overlay and resetting current-node."
         (interactive)
         (let ((current-pos (point)))
           (forward-line -1)
           (when (eq (point) current-pos) ; If we're at the beginning of the buffer
             (goto-char (point-min)))
           (setq-local pb-lisp/current-node nil) ; Reset current node
           (pb-lisp/delete-overlay)              ; Remove highlighting
           (back-to-indentation)))

       (defun pb-lisp/goto-next-sibling-scrolling ()
         "Goto to next sibling, scrolling buffer to keep cursor at the same vertical position in window."
         (interactive)
         (let* ((win-start (window-start))
                (pos-in-win (- (point) win-start))
                (line-pos (count-screen-lines win-start (point))))
           (when (pb-lisp/goto-next-sibling)
             ;; Adjust scroll position to keep cursor at approximately the same screen line
             (recenter line-pos))))

       (defun pb-lisp/goto-previous-sibling-scrolling ()
         "Goto to previous sibling, scrolling buffer to keep cursor at the same vertical position in window."
         (interactive)
         (let* ((win-start (window-start))
                (pos-in-win (- (point) win-start))
                (line-pos (count-screen-lines win-start (point))))
           (when (pb-lisp/goto-prev-sibling)
             ;; Adjust scroll position to keep cursor at approximately the same screen line
             (recenter line-pos)))))


(progn :selection

       (defun pb-lisp/extend-selection-to-next-sibling ()
         "Extend the current selection to include the next sibling node."
         (interactive)
         (let ((nodes (pb-lisp/get-selected-nodes)))
           (if (treesit-node-next-sibling (sq/last nodes) t)
               (pb-lisp/extend-selection)
             (message "Cannot extend selection further"))))

       (defun pb-lisp/extend-selection-to-prev-sibling ()
         "Extend the current selection to include the previous sibling node."
         (interactive)
         (if (pb-lisp/goto-prev-sibling)
             (pb-lisp/extend-selection)
           (message "No previous sibling found")))

       (defun pb-lisp/shrink-selection-from-end ()
         "Shrink the current selection by excluding the last sibling."
         (interactive)
         (if (> pb-lisp/selection-size 1)
             (pb-lisp/shrink-selection)
           (message "Cannot shrink selection further")))

       (defun pb-lisp/shrink-selection-from-beg ()
         "Shrink the current selection by excluding the first sibling."
         (interactive)
         (if (> pb-lisp/selection-size 1)
             (progn (pb-lisp/goto-next-sibling)
                    (pb-lisp/shrink-selection))
           (message "Cannot shrink selection further"))))

(progn :move-expressions

       (defun pb-lisp/copy-selection ()
         "Copy current selection and add it to the kill ring."
         (interactive)
         (let* ((bounds (pb-lisp/selection-bounds))
                (text (buffer-substring-no-properties (car bounds) (cdr bounds))))
           (when bounds
             (kill-new text)
             (message "Copied selection to kill ring"))))

       (defun pb-lisp/swap-siblings_old (direction)
         "Transpose the current node with its next or previous sibling.
          DIRECTION should be 'next or 'prev."
         (let* ((node (pb-lisp/get-current-node))
                (parent (treesit-node-parent node))
                (sibling (cond ((eq direction 'next) (treesit-node-next-sibling node t))
                               ((eq direction 'prev) (treesit-node-prev-sibling node t))
                               (t nil)))
                (node-start (treesit-node-start node))
                (node-end (treesit-node-end node))
                (node-text (buffer-substring-no-properties node-start node-end))
                (sibling-start (and sibling (treesit-node-start sibling)))
                (sibling-end (and sibling (treesit-node-end sibling)))
                (sibling-text (and sibling (buffer-substring-no-properties sibling-start sibling-end))))

           (when (and parent sibling)

             (save-excursion
               ;; We need to handle the order of deletion/insertion differently
               ;; depending on which sibling comes first in the buffer
               (if (< node-start sibling-start)
                   ;; Node comes before sibling
                   (progn
                     (delete-region sibling-start sibling-end)
                     (goto-char sibling-start)
                     (insert node-text)
                     (delete-region node-start node-end)
                     (goto-char node-start)
                     (insert sibling-text))
                 ;; Sibling comes before node
                 (progn
                   (delete-region node-start node-end)
                   (goto-char node-start)
                   (insert sibling-text)
                   (delete-region sibling-start sibling-end)
                   (goto-char sibling-start)
                   (insert node-text))))

             ;; Reindent the region spanning both the original node and sibling
             (let ((indent-region-start (min node-start sibling-start))
                   (indent-region-end (max node-end sibling-end)))
               (indent-region indent-region-start indent-region-end))

             (cond ((eq direction 'next) (pb-lisp/goto-next-sibling))
                   ((eq direction 'prev) (pb-lisp/goto-prev-sibling))))

           (unless sibling
             (message "No %s sibling to swap with" direction))))

       (defun pb-lisp/swap-siblings (direction)
         "Transpose the current node with its next or previous sibling.
          DIRECTION should be 'next or 'prev."
         (let* ((nodes (pb-lisp/get-selected-nodes))
                (first-node (car nodes))
                (parent (treesit-node-parent first-node))
                (last-node (sq/last nodes))
                (start-point (treesit-node-start first-node))
                (end-point (treesit-node-end last-node))
                (sibling (cond ((eq direction 'next) (treesit-node-next-sibling last-node t))
                               ((eq direction 'prev) (treesit-node-prev-sibling first-node t))
                               (t nil)))
                (selection-text (buffer-substring-no-properties start-point end-point))
                (sibling-start (and sibling (treesit-node-start sibling)))
                (sibling-end (and sibling (treesit-node-end sibling)))
                (sibling-text (and sibling (buffer-substring-no-properties sibling-start sibling-end))))
           (when (and parent sibling)

             (save-excursion
               ;; We need to handle the order of deletion/insertion differently
               ;; depending on which sibling comes first in the buffer
               (if (< start-point sibling-start)
                   ;; Node comes before sibling
                   (progn
                     (delete-region sibling-start sibling-end)
                     (goto-char sibling-start)
                     (insert selection-text)
                     (delete-region start-point end-point)
                     (goto-char start-point)
                     (insert sibling-text))
                 ;; Sibling comes before node
                 (progn
                   (delete-region start-point end-point)
                   (goto-char start-point)
                   (insert sibling-text)
                   (delete-region sibling-start sibling-end)
                   (goto-char sibling-start)
                   (insert selection-text))))

             ;; Reindent the region spanning both the original node and sibling
             (let ((indent-region-start (min start-point sibling-start))
                   (indent-region-end (max end-point sibling-end)))
               (indent-region indent-region-start indent-region-end))

             (cond ((eq direction 'next) (pb-lisp/goto-next-sibling))
                   ((eq direction 'prev) (goto-char sibling-start) (pb-lisp/update-overlay))))))

       (defun pb-lisp/swap-with-next-sibling ()
         "Swap the current node with its next sibling."
         (interactive)
         (pb-lisp/swap-siblings 'next))

       (defun pb-lisp/swap-with-prev-sibling ()
         "Swap the current node with its previous sibling."
         (interactive)
         (pb-lisp/swap-siblings 'prev)))

(progn :indentation

       (defun pb-lisp/join-trailing-delimiters (start end)
         "Join trailing delimiters with preceding expressions.
          Closing delimiters should never be the first thing of line, they should be
          close to the last element of the enclosing expression.
          Operates on region between START and END."
         (interactive "r")

         (save-excursion
           (goto-char start)
           (while (re-search-forward "^[ \t]*\\()\\|\\]\\|}\\)" end t)
             (replace-match " \\1" nil nil)
             (backward-char)
             (delete-indentation))))

       (defun pb-lisp/indent-current-node ()
         "Indent the current node."
         (interactive)
         (let* ((node (pb-lisp/get-current-node))
                (start (treesit-node-start node))
                (end (treesit-node-end node)))
           (pb-lisp/join-trailing-delimiters start end)
           (indent-region start end)))

       (defun pb-lisp/indent-parent-node ()
         "Indent the parent node of the current node."
         (interactive)
         (let* ((node (pb-lisp/get-current-node))
                (parent (treesit-node-parent node))
                (current-index (pb-lisp/get-node-child-index node parent)))
           (when parent
             ;; Capture current position in parent's children
             (pb-lisp/goto-parent)
             ;; Indent parent
             (pb-lisp/indent-current-node)
             ;; Return to original child
             (when current-index
               (pb-lisp/goto-nth-child current-index))
             ;; TODO not pretty, arrange this
             (when (eq major-mode 'org-mode)
               (evil-pb-lisp-state -1)
               (evil-pb-lisp-state 1))))))

(progn :edition

       (defun pb-lisp/delete-selection ()
         "Delete current-overlay, adding its content to the kill ring, after deletion goto next node if exists, previous node if exists or parent."
         (interactive)
         (let* ((start (pb-lisp/selection-start))
                (end (pb-lisp/selection-end))
                (node (pb-lisp/get-current-node))
                (parent (treesit-node-parent node))
                (next-sibling (save-excursion (pb-lisp/goto-next-sibling)))
                (prev-sibling (save-excursion (pb-lisp/goto-prev-sibling)))
                ;; Store target position before deleting
                (target-pos (cond (next-sibling
                                   start)
                                  (prev-sibling
                                   (treesit-node-start prev-sibling))
                                  (parent
                                   (treesit-node-start parent))
                                  (t start))))
           ;; Kill the region (adds to kill ring)
           (kill-region start end)
           (cond
            (next-sibling
             ;; Delete forward whitespace
             (while (and (< (point) (point-max))
                         (looking-at-p "[ \t\n]"))
               (delete-char 1)))
            (prev-sibling
             ;; Delete backward whitespace
             (while (and (> (point) (point-min))
                         (progn (backward-char) (looking-at-p "\\s-")))
               (delete-char 1))))
           (goto-char target-pos)
           (pb-lisp/indent-parent-node)))

       (defun pb-lisp/yank-and-select (&optional content)
         (let ((end-point (save-excursion (if content
                                              (insert content)
                                            (yank))
                                          (point))))
           (pb-lisp/set-selection (cons (point) end-point))
           (pb-lisp/indent-parent-node)))

       (defun pb-lisp/yank-after ()
         "Yank clipboard contents after the current node."
         (interactive)
         (let* ((node (pb-lisp/get-current-node))
                (end (treesit-node-end node)))
           (goto-char end)
           (progn
             (if (> (count-lines (treesit-node-start node) (treesit-node-end node)) 1)
                 (insert "\n")
               (insert " ")))
           (pb-lisp/yank-and-select)))

       (defun pb-lisp/yank-before ()
         "Yank clipboard contents before the current node."
         (interactive)
         (let* ((node (pb-lisp/get-current-node))
                (start (treesit-node-start node)))
           (goto-char start)
           (pb-lisp/yank-and-select)))

       (defun pb-lisp/replace-selection (&optional content)
         "Replace the current selection with clipboard contents."
         (interactive)
         (let ((start (pb-lisp/selection-start))
               (end (pb-lisp/selection-end)))
           (delete-region start end)
           (goto-char start)
           (pb-lisp/yank-and-select content)))

       (defun pb-lisp/change-selection ()
         "Delete the current selection and enter insert state."
         (interactive)
         (let ((start (pb-lisp/selection-start))
               (end (pb-lisp/selection-end)))
           (delete-region start end)
           (goto-char start)
           (pb-lisp/indent-parent-node)
           (evil-insert-state 1)))

       (defun pb-lisp/insert-after ()
         "Enter insert state after the current node."
         (interactive)
         (let* ((end (pb-lisp/selection-end)))
           (goto-char end)
           (insert " ")
           (save-excursion
             (when (not (member (char-after) '(?\) ?\] ?\} ?\s ?\t ?\n)))
               (insert " ")))
           (evil-insert-state)))

       (defun pb-lisp/insert-before ()
         "Enter insert state before the current node."
         (interactive)
         (let* ((start (pb-lisp/selection-start)))
           (goto-char start)
           (save-excursion (insert " "))
           ;; (pb-lisp/indent-parent-node)
           (evil-insert-state)))

       (defun pb-lisp/insert-after-with-new-line ()
         "Enter insert state after the current node, adding a new line first."
         (interactive)
         (let* ((end (pb-lisp/selection-end)))
           (goto-char end)
           (insert "\n")
           (back-to-indentation)
           (evil-insert-state)))

       (defun pb-lisp/insert-before-with-new-line ()
         "Enter insert state before the current node, adding a new line first."
         (interactive)
         (let* ((start (pb-lisp/selection-start)))
           (goto-char start)
           (save-excursion
             (beginning-of-line)
             (insert "\n"))
           (forward-line -1)
           (back-to-indentation)
           (evil-insert-state)))

       (defun pb-lisp/insert-at-begining ()
         "Enter insert state at the beginning of the current node's content."
         (interactive)
         (let* ((node (pb-lisp/get-current-node))
                (start (treesit-node-start node)))
           ;; For lists, move inside the opening paren
           (if (> (treesit-node-child-count node t)
                  0)
               (progn
                 (goto-char start)
                 (forward-char 1) ;; Move past the opening delimiter
                 (skip-chars-forward " \t\n") ;; Skip whitespace
                 (evil-insert-state))
             ;; For other node types, go to start
             (goto-char start)
             (evil-insert-state))))

       (defun pb-lisp/insert-at-end ()
         "Enter insert state at the end of the current node's content."
         (interactive)
         (let* ((node (pb-lisp/get-current-node))
                (end (treesit-node-end node)))
           ;; For lists, move inside the closing paren
           (if (> (treesit-node-child-count node t)
                  0)
               (progn
                 (goto-char end)
                 (backward-char 1) ;; Move before the closing delimiter
                 (skip-chars-backward " \t\n") ;; Skip whitespace backward
                 (evil-insert-state))
             ;; For other node types, go to end
             (goto-char end)
             (evil-insert-state))))

       (defun pb-lisp/paren-wrap ()
         "Wrap the current node in parentheses."
         (interactive)
         (let* ((start (pb-lisp/selection-start))
                (end (pb-lisp/selection-end)))
           (save-excursion
             (goto-char end)
             (insert ")")
             (goto-char start)
             (insert "("))
           (pb-lisp/indent-parent-node)
           (goto-char start)))

       (defun pb-lisp/raise-node ()
         "Replace the parent node with the current node."
         (interactive)
         (let* ((node (pb-lisp/get-current-node))
                (parent (treesit-node-parent node))
                (node-start (pb-lisp/selection-start))
                (node-end (pb-lisp/selection-end))
                (node-text (buffer-substring-no-properties node-start node-end)))
           (if parent
               (let ((parent-start (treesit-node-start parent))
                     (parent-end (treesit-node-end parent)))
                 (delete-region parent-start parent-end)
                 (goto-char parent-start)
                 (pb-lisp/yank-and-select node-text))
             (message "Cannot raise - no parent node"))))

       (defun pb-lisp/splice-node ()
         "Remove the current node's delimiters, keeping only its content."
         (interactive)
         (let* ((node (pb-lisp/get-current-node))
                (start (treesit-node-start node))
                (end (treesit-node-end node))
                (first-child (treesit-node-child node 0 t))
                (child-count (treesit-node-child-count node t)))
           (if (and first-child (> child-count 0))
               (save-excursion
                 (let* ((last-child-idx (1- child-count))
                        (last-child (treesit-node-child node last-child-idx t))
                        (content (buffer-substring-no-properties
                                  (1+ start)
                                  (1- end))))
                   (delete-region start end)
                   (goto-char start)
                   (insert content)
                   (indent-region start (+ start (length content)))
                   (pb-lisp/set-selection (cons start (+ start (length content))))))
             (message "Cannot splice - node must have children"))))

       (defun pb-lisp/move-node-down-one-line ()
         "Move the current node or selection down one line."
         (interactive)
         (let* ((start (pb-lisp/selection-start)))
           (goto-char start)
           (insert "\n")
           (pb-lisp/indent-parent-node)))

       (defun pb-lisp/move-node-up-one-line ()
         "Move the current node or selection up one line."
         (interactive)
         (let* ((start (pb-lisp/selection-start))
                (first-non-whitespace-char-of-line
                 (= (point)
                    (save-excursion
                      (back-to-indentation)
                      (point))))
                (empty-line-above
                 (save-excursion
                   (forward-line -1)
                   (beginning-of-line)
                   (looking-at "^[ \t]*$"))))
           (when first-non-whitespace-char-of-line
             (if empty-line-above
                 (progn
                   (save-excursion
                     (forward-line -1)
                     (delete-region (point) (progn (forward-line 1) (point)))))
               (progn
                 (delete-indentation)
                 (forward-char 1)))
             (pb-lisp/indent-parent-node)))))

(progn :evaluation

       (defvar pb-lisp/elisp-methods
         (km :eval
             (lambda (node-text)
               (interactive)
               (message (format "result: %s"
                                (eval (read (concat "(progn " node-text ")"))
                                      t))))

             :eval-pretty
             (lambda (node-text)
               (interactive)
               (pb-elisp/display-expression (eval (read (concat "(progn " node-text ")"))
                                                  t)))))

       (defvar pb-lisp/clojure-methods
         (km :eval
             (lambda (_)
               (interactive)
               (save-excursion
                 (goto-char (pb-lisp/selection-end))
                 (cider-eval-last-sexp)))

             :eval-pretty
             (lambda (code-string)
               (interactive)
               (save-excursion
                 (goto-char (pb-lisp/selection-end))
                 (cider-pprint-eval-last-sexp)))))

       (defvar pb-lisp/org-methods
         (km :eval
             (lambda (node-text)
               (interactive)
               (save-excursion
                 (goto-char (pb-lisp/selection-end))
                 (let* ((lang (pb-org/code-block-language))
                        (treesit-lang (pb-org-babel/lang-string->treesit-lang lang))
                        (methods (alist-get (intern (format "%s-mode" lang)) pb-lisp/major-mode->methods)))
                   (if methods
                       ;; Use language-specific eval method if available
                       (funcall (km/get methods :eval) node-text)
                     ;; Otherwise use standard Org Babel execution
                     (org-babel-execute-src-block)))))

             :eval-pretty
             (lambda (node-text)
               (interactive)
               (save-excursion
                 (goto-char (pb-lisp/selection-end))
                 (let* ((lang (pb-org/code-block-language))
                        (treesit-lang (pb-org-babel/lang-string->treesit-lang lang))
                        (methods (alist-get (intern (format "%s-mode" lang)) pb-lisp/major-mode->methods)))
                   (if methods
                       ;; Use language-specific pretty eval method if available
                       (funcall (km/get methods :eval-pretty) node-text)
                     ;; Otherwise use standard Org Babel execution with result display
                     (progn
                       (org-babel-execute-src-block)
                       (org-babel-open-src-block-result))))))))

       (defvar pb-lisp/major-mode->methods
         `((emacs-lisp-mode ,@pb-lisp/elisp-methods)
           (clojure-mode ,@pb-lisp/clojure-methods)
           (clojurescript-mode ,@pb-lisp/clojure-methods)
           (clojurec-mode ,@pb-lisp/clojure-methods)
           (org-mode ,@pb-lisp/org-methods))
         "Maps tree-sitter language symbols to a plist of methods/configs.
          Each language entry contains:
          - :parser-lang - the language symbol for the tree-sitter parser
          - :get-node - the string name used when getting nodes with treesit-node-at
          - :modes - list of major modes associated with this language")

       (setq pb-lisp/major-mode->methods
             `((emacs-lisp-mode ,@pb-lisp/elisp-methods)
               (clojure-mode ,@pb-lisp/clojure-methods)
               (clojurescript-mode ,@pb-lisp/clojure-methods)
               (clojurec-mode ,@pb-lisp/clojure-methods)
               (org-mode ,@pb-lisp/org-methods)))

       (defun pb-lisp/get-method (k)
         (km/get (alist-get major-mode pb-lisp/major-mode->methods)
                 k))

       (defun pb-lisp/eval-current-node ()
         "Evaluate the current tree-sitter node in the appropriate context."
         (interactive)
         (funcall (pb-lisp/get-method :eval)
                  (pb-lisp/current-selection-as-string)))

       (defun pb-lisp/eval-pretty ()
         "Evaluate and pretty-print the current Lisp expression.
          Displays the result in a buffer named 'ELisp_eval'."
         (interactive)
         (funcall (pb-lisp/get-method :eval-pretty)
                  (pb-lisp/current-selection-as-string))))

(progn :motion-advice

       (defvar pb-lisp/motion-functions
         '(pb-lisp/goto-first-child
           pb-lisp/goto-parent
           pb-lisp/goto-last-child
           pb-lisp/goto-next-sibling
           pb-lisp/goto-prev-sibling
           pb-lisp/goto-first-sibling
           pb-lisp/goto-last-sibling
           pb-lisp/goto-nth-child))

       (defun pb-lisp/add-navigation-advice (&rest args)
         (dolist (func pb-lisp/motion-functions)
           (apply #'advice-add func args)))

       (defun pb-lisp/remove-navigation-advice (f)
         "Remove widening advice from all motion functions."
         (interactive)
         (dolist (func pb-lisp/motion-functions)
           (advice-remove func f))))

(progn :header-line

       ;; Variable for header line format
       (defvar-local pb-lisp/header-line-format nil
         "Format string for the header line in pb-lisp narrow mode.")

       (defvar-local pb-lisp/header-line-mode nil)

       ;; Function to get the keyword from a progn node
       (defun pb-lisp/get-progn-keyword (node)
         "Extract the keyword from a progn node if available."
         (when (and node (not (string= "source_file" (treesit-node-type node))))
           (let ((node-text (treesit-node-text node)))
             (when (string-match "^(progn[[:space:]\n]+\\(:[^[:space:]\n]+\\)" node-text)
               (match-string 1 node-text)))))

       ;; Function to format the header line
       (defun pb-lisp/update-header-line (&rest _)
         "Update the header line to show the current position in the code tree."
         (interactive)
         (when pb-lisp/header-line-mode
           (let* ((node (pb-lisp/get-current-node))
                  (path (pb-lisp/build-code-path node))
                  (path-str (if path
                                (mapconcat #'identity path " > ")
                              "Top level")))
             ;; (print (kmq path node))
             (setq-local pb-lisp/header-line-format
                         (propertize path-str 'face 'outline-2))
             (setq header-line-format pb-lisp/header-line-format))))

       ;; Function to build the path from root to current node
       (defun pb-lisp/build-code-path (node)
         "Build a path of progn keywords from root to the current node."
         (let ((path '())
               (current-node (treesit-node-parent node)))
           (while current-node
             (when-let ((keyword (pb-lisp/get-progn-keyword current-node)))
               (push keyword path))
             (setq current-node (treesit-node-parent current-node)))
           path))

       (pb-lisp/add-navigation-advice :after #'pb-lisp/update-header-line))

(progn :narrow-mode

       ;; Flag to track if we're in narrow mode
       (defvar-local pb-lisp/narrow-mode nil
         "When non-nil, automatically narrow to current node when navigating.")

       ;; Toggle for narrow mode
       (defun pb-lisp/toggle-narrow-mode ()
         "Toggle the automatic narrowing of nodes when navigating."
         (interactive)
         (setq-local pb-lisp/narrow-mode (not pb-lisp/narrow-mode))
         (if pb-lisp/narrow-mode
             (progn
               (pb-lisp/narrow-to-current)
               (message "Narrow mode enabled - navigating will stay narrowed to current node"))
           (widen)
           (message "Narrow mode disabled")))

       ;; Narrow to current node
       (defun pb-lisp/narrow-to-current ()
         "Narrow buffer to the current node."
         (interactive)
         (widen) ;; Ensure we're not already narrowed
         (let ((start (pb-lisp/selection-start))
               (end (pb-lisp/selection-end)))
           (narrow-to-region start end)
           (pb-lisp/indent-current-node)))

       (pb/comment
        (advice-remove 'widen #'pb-lisp/indent-current-node)
        (advice-remove 'widen #'pb-lisp/indent-parent-node))

       ;; Advice to update narrowing after navigation
       (defun pb-lisp/maybe-narrow-after-navigation (&rest _)
         "If narrow-mode is enabled, narrow to the current node after navigation."
         (when pb-lisp/narrow-mode
           (pb-lisp/narrow-to-current)))

       ;; Advise navigation functions to handle narrowing
       (advice-add 'pb-lisp/update-overlay :after #'pb-lisp/maybe-narrow-after-navigation)

       (progn :motion-with-widening

              ;; Define advice to temporarily widen buffer before navigation
              (defun pb-lisp/widen-before-navigation-advice (orig-fun &rest args)
                "Temporarily widen the buffer before navigation if narrow mode is active."
                (when pb-lisp/narrow-mode
                  (widen))
                (apply orig-fun args))

              (pb-lisp/add-navigation-advice :around #'pb-lisp/widen-before-navigation-advice)

              '(pb-lisp/remove-navigation-advice #'pb-lisp/widen-before-navigation-advice)))

(progn :bindings

       (defun pb-lisp/exit ()
         (interactive)
         (evil-pb-lisp-state -1)
         (evil-normal-state 1)
         (when (eq major-mode 'org-mode)
           (evil-sorg-state 1)))

       (defvar pb-lisp/bindings
         (list (kbd "<escape>") #'pb-lisp/exit
               (kbd "<tab>") #'pb-lisp/indent-current-node
               (kbd "<return>") #'pb-lisp/move-node-down-one-line
               (kbd "S-<return>") #'pb-lisp/move-node-up-one-line
               "h" #'pb-lisp/goto-prev-sibling
               "l" #'pb-lisp/goto-next-sibling
               "j" #'pb-lisp/enter-node
               "k" #'pb-lisp/goto-parent
               ;; sibling moves, fixed cursor
               (kbd "C-S-l") #'pb-lisp/goto-next-sibling-scrolling
               (kbd "C-S-h") #'pb-lisp/goto-previous-sibling-scrolling
               ;; first and last
               (kbd "C-l") #'pb-lisp/goto-last-sibling
               (kbd "C-h") #'pb-lisp/goto-first-sibling
               ;; line moves
               (kbd "C-j") #'pb-lisp/goto-next-line
               (kbd "C-k") #'pb-lisp/goto-previous-line

               "L" #'pb-lisp/extend-selection-to-next-sibling
               "H" #'pb-lisp/extend-selection-to-prev-sibling
               "J" #'pb-lisp/shrink-selection-from-beg
               "K" #'pb-lisp/shrink-selection-from-end

               (kbd "M-l") #'pb-lisp/swap-with-next-sibling
               (kbd "M-h") #'pb-lisp/swap-with-prev-sibling
               (kbd "M-j") #'pb-lisp/paren-wrap
               (kbd "M-k") #'pb-lisp/raise-node

               "x" #'pb-lisp/delete-selection
               "p" #'pb-lisp/yank-after
               "P" #'pb-lisp/yank-before
               "y" #'pb-lisp/copy-selection
               "R" #'pb-lisp/replace-selection
               "s" #'pb-lisp/splice-node

               "c" #'pb-lisp/change-selection
               "A" #'pb-lisp/insert-after
               "O" #'pb-lisp/insert-before-with-new-line
               "o" #'pb-lisp/insert-after-with-new-line
               "a" #'pb-lisp/insert-at-end
               "I" #'pb-lisp/insert-before
               "i" #'pb-lisp/insert-at-begining
               "e" #'pb-lisp/eval-current-node
               (kbd "C-e") #'pb-lisp/eval-pretty

               "t" #'pb-misc/toggle-hiding
               (kbd "C-t") #'hs-hide-level
               (kbd "q r") #'pb-lisp/gptel-request-replace
               "?" #'pb-lisp/log-node))

       (dolist (binding (sq/partition 2 2 pb-lisp/bindings))
         (evil-define-key* nil
           evil-pb-lisp-state-map
           (car binding)
           (cadr binding))))

(progn :font-lock

       (progn :definition

              (defface font-lock-function-definition-face
                '((t :inherit font-lock-function-name-face))
                "Face used for function names in definition forms like defun, defmacro, etc."
                :group 'font-lock-faces)

              ;; First, create a common face for all definition forms
              (defface pb-lisp/definition-face
                '((t :inherit font-lock-function-definition-face
                   :weight semi-bold))
                "Face used for symbols in definition forms (def* forms)."
                :group 'font-lock-faces)

              ;; Pattern for matching def* forms in Elisp and Clojure
              (defconst pb-lisp/def-form-pattern
                "(\\(\\<\\(?:def\\w*\\|ns\\)\\)\\_>[ \t']*\\(\\(?:\\sw\\|\\s_\\)+\\)"
                "Regexp to match common Elisp and Clojure definition forms and capture the defined symbol.
                 Matches all symbols starting with 'def', like defun, defmacro, defsubst, defadvice, defn, defvar, etc.,
                 as well as Clojure's 'ns' namespace declarations.")

              (quote
               ;; this is working on elisp defun, unlike the def-form-pattern stuff
               (font-lock-remove-keywords
                'emacs-lisp-mode
                '(("(\\(def\\(?:un\\|macro\\|subst\\|advice\\|ine-\\w+\\)\\)\\_>[ \t']*\\(\\(?:\\sw\\|\\s_\\)+\\)"
                   (2 'font-lock-function-definition-face))
                  ("(\\(define-derived-mode\\)\\_>[ \t']*\\(\\(?:\\sw\\|\\s_\\)+\\)"
                   (2 'font-lock-variable-name-face)))))

              (dolist (mode pb-lisp/modes)
                (font-lock-add-keywords
                 mode
                 `((,pb-lisp/def-form-pattern (1 font-lock-keyword-face)
                    (2 'pb-lisp/definition-face)))))

              (dolist (mode pb-lisp/modes)
                (font-lock-add-keywords
                 mode
                 `((,pb-lisp/def-form-pattern (1 font-lock-keyword-face)
                    (2 (list :weight 'bold))))
                 1)))

       (progn :package-prefix

              (defface pb-lisp/namespace-prefix-face
                `((t
                   :foreground ,(pb-color (doom-color 'red) (desaturate .8) (darken 0))
                   :weight normal))
                "Face for namespace prefixes in elisp (text before a slash in a symbol).")

              (defun pb-lisp/prefix-matcher (limit)
                "Match symbols like 'prefix/name' up to LIMIT, capturing prefix part."
                (when (re-search-forward "\\<\\([a-zA-Z][a-zA-Z0-9-.]*\\|:+[a-zA-Z][a-zA-Z0-9-.]*\\)\\(/\\)" limit t)
                  ;; We found a match - the \\< ensures we match at word boundaries
                  ;; We've added a second capture group for the slash
                  (let ((prefix-beginning (match-beginning 1))
                        (prefix-end (match-end 1))
                        (slash-beginning (match-beginning 2))
                        (slash-end (match-end 2)))
                    ;; Set match data to highlight both the prefix and the slash
                    ;; The full match will be from prefix beginning to slash end
                    (set-match-data (list prefix-beginning slash-end ;; full match
                                          prefix-beginning prefix-end ;; group 1 (prefix)
                                          slash-beginning slash-end)) ;; group 2 (slash)
                    t)))

              (dolist (mode pb-lisp/modes)
                (font-lock-add-keywords
                 mode
                 '((pb-lisp/prefix-matcher 1 'pb-lisp/namespace-prefix-face prepend)
                   (pb-lisp/prefix-matcher 2 'default prepend))
                 1))

              (pb/comment
               (font-lock-remove-keywords
                'emacs-lisp-mode
                '((pb-lisp/prefix-matcher 0 'pb-lisp/namespace-prefix-face prepend)
                  (pb-lisp/prefix-matcher 0 'pb-lisp/namespace-prefix-face append)
                  (pb-lisp/prefix-matcher 0 'pb-lisp/namespace-prefix-face keep)
                  (pb-lisp/prefix-matcher 0 'pb-lisp/namespace-prefix-face)))

               (font-lock-flush))

              font-lock-keywords-alist)

       (progn :progn-section-keyword

              (defface pb-lisp/progn-keyword-face
                `((t
                   :foreground ,(doom-blend 'red 'blue 0.7)
                   ;; (pb-color (doom-color 'red) (desaturate .8) (darken 0.1))
                   :weight bold
                   :height 1.2))
                "Face for keywords in progn sections like (progn :keyword).")

              (defun pb-lisp/progn-keyword-matcher (limit)
                "Match keywords after a progn form up to LIMIT, like (progn :keyword)."
                (when (re-search-forward "(\\s-*\\(progn\\|do\\)\\s-+\\(:[a-zA-Z][a-zA-Z0-9/-]*\\)" limit t)
                  (let ((keyword-beginning (match-beginning 2))
                        (keyword-end (match-end 2)))
                    (set-match-data (list keyword-beginning keyword-end
                                          keyword-beginning keyword-end))
                    t)))

              (dolist (mode pb-lisp/modes)
                (font-lock-add-keywords
                 mode
                 '((pb-lisp/progn-keyword-matcher 1 'pb-lisp/progn-keyword-face prepend))
                 1))

              (defun pb-lisp/highlight-progn-sections ()
                "Refresh keyword highlighting in the current buffer."
                (interactive)
                (when (derived-mode-p 'emacs-lisp-mode)
                  (font-lock-flush)))

              ;; Automatically apply highlighting to open buffers
              (dolist (buffer (buffer-list))
                (with-current-buffer buffer
                  (when (derived-mode-p 'emacs-lisp-mode)
                    (font-lock-flush)))))

       '(progn :re-frame
               (defvar pb-lisp/re-frame-keyword-pattern
                 "\\(::+\\)\\([a-zA-Z][a-zA-Z0-9/-]*\\)\\>"
                 "Regexp to match re-frame style double-colon keywords without namespace.")

               ;; Example keyword for testing
               ::pouet

               ;; Use a higher priority (2) to override potential conflicts
               (dolist (mode pb-lisp/modes)
                 (font-lock-add-keywords
                  mode
                  `((,pb-lisp/re-frame-keyword-pattern
                     (1 (list :weight 'bold))
                     (2 (list :weight 'bold))))
                  2))))

(progn :gptel-current-node

       (require 'pb-gptel)

       (defun pb-lisp/gptel-request-replace (&optional options)
         (interactive)
         (pb/let [(km/keys prompt callback) options]
           (pb-gptel/request

            (km :context
                (km :current-file
                    (km :editor "emacs"
                        :buffer-name (buffer-file-name)
                        :major-mode (symbol-name major-mode)
                        :file-content (buffer-substring-no-properties (point-min) (point-max)))
                    :additional-files
                    (pb-gptel/context-files-to-km))
                :instructions
                (km :base "You are a useful code assistant, you really like lisp-like languages and you know how to balance parentheses correctly."
                    :response-format ["Your response should be valid code, intended to replace the current expression in a source code file."
                                      "Don't use markdown code block syntax or any non-valid code in your output."]
                    :expression (pb-lisp/current-selection-as-string)
                    :task (or prompt
                              (read-string "Edit current selection: "))))

            (km :callback
                (or callback
                    (lambda (res info)
                      (pb-lisp/replace-selection res))))))))

(message "pb-lisp (treesit) loaded")

(provide 'pb-lisp)

