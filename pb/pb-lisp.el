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

(progn :mode-definition

       (evil-define-state pb-lisp
         "PB Lisp state."
         :tag " PB_LISP "
         :message "-- PB_LISP --"
         :enable (normal)
         :entry-hook (pb-lisp/enter-mode)
         :exit-hook (pb-lisp/exit-mode))

       (setq evil-pb-lisp-state-cursor
             `(box "magenta"))

       (defvar pb-lisp/modes
         '(org-mode clojure-mode clojurescript-mode clojurec-mode
           emacs-lisp-mode fennel-mode sheme-mode racket-mode))

       (defvar-local pb-lisp/current-node nil)

       (defvar pb-lisp/major-mode->treesit-lang
         '((emacs-list . elisp)
           (clojure-mode . clojure)
           (clojurescript-mode . clojure)
           (clojurec-mode . clojure)
           (org-mode . org)))

       (defun pb-lisp/parser-setup ()
         "Setup tree-sitter parser for current elisp buffer."
         (when-let ((lang (alist-get major-mode pb-lisp/major-mode->treesit-lang)))
           (print (cons "parser setup " lang))
           (when (treesit-language-available-p lang)
             (treesit-parser-create lang))))

       (defun pb-lisp/enter-mode ()
         "Run when on entering sorg mode."
         (when (member major-mode pb-lisp/modes)
           (pb-lisp/parser-setup)
           (hl-line-mode -1)
           (setq pb-lisp/selection-count 0)
           (goto-char (car (pb-lisp/get-current-node-bounds)))
           (pb-lisp/select-current-node)))

       (defun pb-lisp/exit-mode ()
         "Run on exiting sorg mode."
         (print "exit pb-lisp")
         (setq-local pb-lisp/current-node nil)
         (pb-lisp/delete-overlay)
         (hl-line-mode 1)))

(progn :selection

       (defvar pb-lisp/selection)

       (defun pb-lisp/set-selection (bounds)
         (setq pb-lisp/selection bounds)
         (pb-lisp/update-overlay))

       (defun pb-lisp/select-current-node ()
         (pb-lisp/set-selection
          (pb-lisp/get-current-node-bounds)))

       (progn :overlay

              (defface pb-lisp/overlay-face
                '((t
                   :inherit symex--current-node-face
                   :extend nil :background "#3b3042"))
                "Face used to highlight the current tree node."
                :group 'pb-lisp/faces)

              (defvar pb-lisp/overlays ()
                "List of active overlays used to highlight Lisp nodes.")

              (defun pb-lisp/delete-overlay ()
                "Delete all highlight overlays from the current buffer."
                (mapc #'delete-overlay pb-lisp/overlays)
                (setq pb-lisp/overlays nil))

              (defun pb-lisp/update-overlay ()
                "Update the highlight overlay to match the start/end position of NODE."
                (interactive)
                (pb-lisp/delete-overlay)
                (let ((overlay (make-overlay (car pb-lisp/selection)
                                             (cdr pb-lisp/selection))))
                  (overlay-put overlay 'face 'pb-lisp/overlay-face)
                  (push overlay pb-lisp/overlays)))


              (progn :indented-overlays

                     (defun pb-lisp/update-overlay2 ()
                       "Update the highlight overlay based on the current selection.
                       Creates overlays to highlight the region between the start and end
                       positions specified in `pb-lisp/selection`. Handles multi-line
                       selections by creating a separate overlay for each line in the
                       selected region."
                       (interactive)
                       (pb-lisp/delete-overlay)
                       (let* ((beg (car pb-lisp/selection))
                              (end (cdr pb-lisp/selection))
                              (column-beg (save-excursion (goto-char beg) (current-column)))
                              (column-end (save-excursion (goto-char end) (current-column)))
                              (column-start (min column-beg column-end))
                              (line-beg (line-number-at-pos beg))
                              (line-end (line-number-at-pos end)))

                         ;; Create an overlay for each line in the rectangle
                         (save-excursion
                           (goto-char (point-min))
                           (forward-line (1- line-beg))
                           (while (<= (line-number-at-pos) line-end)
                             (let ((start (progn (move-to-column column-start t) (point)))
                                   (end (progn (end-of-line) (min end (point)))))
                               (let ((overlay (make-overlay start (max (1+ start) end))))
                                 (overlay-put overlay 'face 'pb-lisp/overlay-face)
                                 (push overlay pb-lisp/overlays)))
                             (forward-line 1)))))

                     (defun pb-lisp/update-overlay3 ()
                       "Update the highlight overlay based on the current selection with improved handling.
                       Creates overlays to highlight the region between the start and end
                       positions specified in `pb-lisp/selection`. Intelligently handles multi-line
                       selections by creating appropriate overlays for each line, ensuring proper
                       highlighting even when subsequent lines start before the column-start position."
                       (interactive)
                       (pb-lisp/delete-overlay)
                       (let* ((beg (car pb-lisp/selection))
                              (end (cdr pb-lisp/selection))
                              (column-beg (save-excursion (goto-char beg) (current-column)))
                              (column-end (save-excursion (goto-char end) (current-column)))
                              (column-start (min column-beg column-end))
                              (line-beg (line-number-at-pos beg))
                              (line-end (line-number-at-pos end)))

                         ;; Create an overlay for each line in the rectangle
                         (save-excursion
                           (goto-char (point-min))
                           (forward-line (1- line-beg))

                           ;; Handle first line specially - always highlight from beg to end of line
                           (let* ((line-end-pos (line-end-position))
                                  (effective-end (min end line-end-pos))
                                  (overlay (make-overlay beg (max (1+ beg) effective-end))))
                             (overlay-put overlay 'face 'pb-lisp/overlay-face)
                             (push overlay pb-lisp/overlays))

                           ;; Handle subsequent lines
                           (forward-line 1)
                           (while (<= (line-number-at-pos) line-end)
                             (let* ((line-start-column (save-excursion
                                                         (back-to-indentation)
                                                         (current-column)))
                                    (line-end-pos (line-end-position))
                                    (effective-start (progn
                                                       (move-to-column (if (lispy--empty-line-p)
                                                                           column-start
                                                                         (min line-start-column column-start))
                                                                       t)
                                                       (point)))
                                    (effective-end (min end line-end-pos)))
                               (when (<= effective-end line-end-pos) ; Ensure we don't go past EOL
                                 (let ((overlay (make-overlay effective-start (max (1+ effective-start) effective-end))))
                                   (overlay-put overlay 'face 'pb-lisp/overlay-face)
                                   (push overlay pb-lisp/overlays))))
                             (forward-line 1))))))))

(progn :current-node

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

       (defun pb-lisp/get-current-node (&optional fresh)
         "Get the tree-sitter node at point."
         (if (treesit-parser-list)
             (or (and (not fresh) pb-lisp/current-node)
                 (let* ((node (treesit-node-at (point)
                                               (alist-get major-mode pb-lisp/major-mode->treesit-lang)))
                        (node (if (member (treesit-node-type node) '(")" "]" "}"))
                                  (treesit-node-parent node)
                                node)))
                   (pb-lisp/get-topmost-node node)))
           (message "tree-sit not enabled")))

       (defun pb-lisp/refresh-current-node ()
         "Get the tree-sitter node at point."
         (setq-local pb-lisp/current-node (pb-lisp/get-current-node t)))

       (defun pb-lisp/clear-current-node ()
         "Clear the current node selection.
          Sets `pb-lisp/current-node' to nil for the current buffer,
          effectively removing any node selection state."
         (setq-local pb-lisp/current-node nil))

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

       (defun pb-lisp/log-node (&optional node)
         (interactive)
         (let* ((node (or node (pb-lisp/get-current-node)))
                (parent (treesit-node-parent node)))
           (pb-elisp_display-expression
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
                                  (treesit-node-children node)))
            #'km_pp)))

       (defun pb-lisp/current-selection-as-string ()
         "Get the string content of the current selection overlay.
         Returns the text within the bounds of `pb-lisp/current-overlay', or
         nil if the overlay is not properly set or has invalid bounds."
         (let* ((start (car pb-lisp/selection))
                (end (cdr pb-lisp/selection)))
           (when (and start end)
             (buffer-substring-no-properties start end)))))

(progn :motion

       (defvar pb-lisp/skipped-node-types
         '("\n" ")" "]" "}"))

       (defun pb-lisp/goto-node (node message)
         "Go to the start position of NODE or display MESSAGE if node is nil."
         (if node
             (progn (goto-char (treesit-node-start node))
                    (setq-local pb-lisp/current-node node)
                    (pb-lisp/set-selection (cons (treesit-node-start node) (treesit-node-end node)))
                    node)
           (progn (message message)
                  nil)))

       (defun pb-lisp/goto-parent ()
         "Move to the beginning of the parent node."
         (interactive)
         (let* ((node (pb-lisp/get-current-node))
                (parent (treesit-node-parent node)))
           (pb-lisp/goto-node parent "No parent node found")))

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

       (defun pb-lisp/goto-first-child ()
         "Move to the first child of current node."
         (interactive)
         (let* ((node (pb-lisp/get-current-node))
                (child (and node (treesit-node-child node 0 t))))
           (pb-lisp/goto-node (or child node) "No child node found")))

       (defun pb-lisp/goto-last-child ()
         "Move to the last child of current node."
         (interactive)
         (let* ((node (pb-lisp/get-current-node))
                (count (treesit-node-child-count node))
                (child (and (> count 0) (treesit-node-child node (1- count)))))
           (pb-lisp/goto-node child "No child node found"))))

(progn :selection

       (defun pb-lisp/extend-selection-to-next-sibling ()
         "Extend the current selection to include the next sibling node."
         (interactive)
         (let* ((start (car pb-lisp/selection))
                ;; Go to the end position of the overlay before finding next sibling
                (end (cdr pb-lisp/selection))
                (node (pb-lisp/get-current-node))
                (next-sibling (let ((sibling node))
                                (while (and sibling
                                            (<= (treesit-node-end sibling) end))
                                  (setq sibling (treesit-node-next-sibling sibling t)))
                                sibling)))
           (goto-char start)
           (if next-sibling
               (pb-lisp/set-selection (cons start (treesit-node-end next-sibling)))
             (message "No next sibling found"))))

       (defun pb-lisp/extend-selection-to-prev-sibling ()
         "Extend the current selection to include the previous sibling node."
         (interactive)
         (let* ((start (car pb-lisp/selection))
                (end (cdr pb-lisp/selection))
                ;; Go to the start position of the overlay before finding prev sibling
                (_ (goto-char start))
                (node (pb-lisp/get-current-node))
                (prev-sibling (let ((sibling node))
                                (while (and sibling
                                            (>= (treesit-node-start sibling) start))
                                  (setq sibling (treesit-node-prev-sibling sibling t)))
                                sibling))
                (new-start (treesit-node-start prev-sibling)))
           (goto-char new-start)
           (if prev-sibling
               (pb-lisp/set-selection (cons new-start end))
             (message "No previous sibling found"))))

       (defun pb-lisp/get-selected-nodes ()
         "Get all nodes covered by the current selection overlay.
         Returns a list of (parent child-count node-list selected-indices) where:
         - parent is the parent node
         - child-count is the number of children
         - node-list is the list of named children
         - selected-indices is a cons of (first-selected-index . last-selected-index)"
         (let* ((start (car pb-lisp/selection))
                (end (cdr pb-lisp/selection))
                (node (pb-lisp/get-current-node))
                (parent (treesit-node-parent node))
                (child-count (and parent (treesit-node-child-count parent t)))
                nodes selected-nodes first-idx last-idx)

           (when (and parent child-count (> child-count 0))
             ;; Get all child nodes
             (setq nodes (cl-loop for i from 0 below child-count
                                  collect (treesit-node-child parent i t)))

             ;; Find which nodes are in our selection
             (setq selected-nodes
                   (cl-loop for node in nodes
                            for i from 0
                            for node-start = (treesit-node-start node)
                            for node-end = (treesit-node-end node)
                            when (and (<= start node-end) (>= end node-start))
                            collect (cons i node)))

             (when selected-nodes
               (setq first-idx (caar selected-nodes)
                     last-idx (caar (last selected-nodes)))

               ;; Print information about selected nodes
               (cl-loop for (idx . node) in selected-nodes
                        for text = (buffer-substring-no-properties
                                    (treesit-node-start node)
                                    (treesit-node-end node))
                        do (message "  Node %d: %s [%s]"
                                    idx
                                    (treesit-node-type node)
                                    (if (> (length text) 30)
                                        (concat (substring text 0 27) "...")
                                      text)))

               ;; (print nodes)

               (let ((first-node (nth first-idx nodes))
                     (last-node (nth last-idx nodes)))
                 (km :parent parent
                     :child-count child-count
                     :nodes nodes
                     :first-node first-node
                     :last-node last-node
                     :indices (cons first-idx last-idx)
                     :bounds (cons (treesit-node-start first-node)
                                   (treesit-node-end last-node))))))))

       (defun pb-lisp/shrink-selection-from-end ()
         "Shrink the current selection by excluding the last sibling."
         (interactive)
         (pb_let [(km_keys parent nodes indices) (pb-lisp/get-selected-nodes)
                  start (car pb-lisp/selection)]
           (if (and parent (> (cdr indices) (car indices)))
               (let* ((new-last-idx (1- (cdr indices)))
                      (new-last-node (nth new-last-idx nodes))
                      (new-end (treesit-node-end new-last-node)))
                 (pb-lisp/set-selection (cons start new-end)))
             (message "Cannot shrink selection further"))))

       (defun pb-lisp/shrink-selection-from-beg ()
         "Shrink the current selection by excluding the first sibling."
         (interactive)
         (pb_let [(km_keys parent nodes indices) (pb-lisp/get-selected-nodes)
                  end (cdr pb-lisp/selection)]
           (if (and parent (< (car indices) (cdr indices)))
               (let* ((new-first-idx (1+ (car indices)))
                      (new-first-node (nth new-first-idx nodes))
                      (new-start (treesit-node-start new-first-node)))
                 (goto-char new-start)
                 (pb-lisp/set-selection (cons new-start end)))
             (message "Cannot shrink selection further")))))

(progn :move-expressions

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
             (pb-lisp/clear-current-node)
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
         (pb_let [(km_keys parent last-node first-node bounds) (pb-lisp/get-selected-nodes)
                  (cons start-point end-point) bounds
                  sibling (cond ((eq direction 'next) (treesit-node-next-sibling last-node t))
                                ((eq direction 'prev) (treesit-node-prev-sibling first-node t))
                                (t nil))
                  selection-text (buffer-substring-no-properties start-point end-point)
                  sibling-start (and sibling (treesit-node-start sibling))
                  sibling-end (and sibling (treesit-node-end sibling))
                  sibling-text (and sibling (buffer-substring-no-properties sibling-start sibling-end))]
           (print (kmq bounds
                       selection-text))
           (when (and parent sibling)
             (pb-lisp/clear-current-node)
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
                   ((eq direction 'prev) (pb-lisp/goto-prev-sibling))))))

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
         (pb-lisp/clear-current-node)
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

       (defun pb-lisp/indent-parent-node (&optional clear-current-node)
         "Indent the parent node of the current node."
         (interactive)
         (when clear-current-node
           (pb-lisp/clear-current-node))
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
               (pb-lisp/goto-nth-child current-index)
               (goto-char (treesit-node-start node)))))))

(progn :edition

       '(a (erter
            dfg)
         (ert
          dfg))

       (defun pb-lisp/copy-selection ()
         "Copy current selection and add it to the kill ring."
         (interactive)
         (let* ((start (car pb-lisp/selection))
                (end (cdr pb-lisp/selection))
                (text (buffer-substring-no-properties start end)))
           (when (and start end)
             (kill-new text)
             (message "Copied selection to kill ring"))))

       (defun pb-lisp/delete-selection ()
         "Delete current-overlay, adding its content to the kill ring, after deletion goto next node if exists, previous node if exists or parent."
         (interactive)
         (let* ((start (car pb-lisp/selection))
                (end (cdr pb-lisp/selection))
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
           (pb-lisp/indent-parent-node t)
           (pb-lisp/select-current-node)))

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
           (save-excursion (yank))
           (pb-lisp/indent-parent-node t)
           (pb-lisp/select-current-node)))

       (defun pb-lisp/yank-before ()
         "Yank clipboard contents before the current node."
         (interactive)
         (let* ((node (pb-lisp/get-current-node))
                (start (treesit-node-start node)))
           (goto-char start)
           (save-excursion
             (yank)
             (insert " "))
           (pb-lisp/indent-parent-node t)
           (pb-lisp/select-current-node)))

       (defun pb-lisp/replace-selection (&optional content)
         "Replace the current selection with clipboard contents."
         (interactive)
         (let ((start (car pb-lisp/selection))
               (end (cdr pb-lisp/selection)))
           (delete-region start end)
           (goto-char start)
           (save-excursion (if content (insert content) (yank)))
           (pb-lisp/indent-parent-node t)
           (pb-lisp/set-selection
            (cons start
                  (if content
                      (+ start (length content))
                    end)))))

       (defun pb-lisp/change-selection ()
         "Delete the current selection and enter insert state."
         (interactive)
         (let ((start (car pb-lisp/selection))
               (end (cdr pb-lisp/selection)))
           (delete-region start end)
           (goto-char start)
           (pb-lisp/indent-parent-node t)
           (evil-insert-state 1)))

       (defun pb-lisp/insert-after ()
         "Enter insert state after the current node."
         (interactive)
         (let* ((node (pb-lisp/get-current-node))
                (end (treesit-node-end node)))
           (goto-char end)
           (insert " ")
           (save-excursion
             (when (member (char-after) '(?\( ?\[ ?\{))
               (insert " ")))
           (pb-lisp/indent-parent-node t)
           (evil-insert-state)))

       (defun pb-lisp/insert-before ()
         "Enter insert state before the current node."
         (interactive)
         (let* ((node (pb-lisp/get-current-node))
                (start (treesit-node-start node)))
           (goto-char start)
           (save-excursion (insert " "))
           (pb-lisp/indent-parent-node t)
           (evil-insert-state)))

       (defun pb-lisp/insert-at-begining ()
         "Enter insert state at the beginning of the current node's content."
         (interactive)
         (let* ((node (pb-lisp/get-current-node))
                (start (treesit-node-start node))
                (node-type (treesit-node-type node)))
           (pb-lisp/clear-current-node)
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
                (end (treesit-node-end node))
                (node-type (treesit-node-type node)))
           ;; (print node-type)
           (pb-lisp/clear-current-node)
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
         (let* ((node (pb-lisp/get-current-node))
                (start (treesit-node-start node))
                (end (treesit-node-end node)))
           (save-excursion
             (goto-char end)
             (insert ")")
             (goto-char start)
             (insert "("))
           (pb-lisp/indent-parent-node t)
           (pb-lisp/select-current-node)
           (goto-char start)))

       (defun pb-lisp/raise-node ()
         "Replace the parent node with the current node."
         (interactive)
         (let* ((node (pb-lisp/get-current-node))
                (parent (treesit-node-parent node))
                (node-start (treesit-node-start node))
                (node-end (treesit-node-end node))
                (node-text (buffer-substring-no-properties node-start node-end)))
           (if parent
               (let ((parent-start (treesit-node-start parent))
                     (parent-end (treesit-node-end parent)))
                 (delete-region parent-start parent-end)
                 (goto-char parent-start)
                 (save-excursion (insert node-text))
                 (pb-lisp/indent-parent-node t)
                 (pb-lisp/select-current-node))
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
                   (pb-lisp/clear-current-node)
                   (delete-region start end)
                   (goto-char start)
                   (insert content)
                   (indent-region start (+ start (length content)))
                   (pb-lisp/set-selection (cons start (+ start (length content))))))
             (message "Cannot splice - node must have children"))))

       (defun pb-lisp/move-node-down-one-line ()
         "Move the current node or selection down one line."
         (interactive)
         (let* ((node (pb-lisp/get-current-node))
                (start (treesit-node-start node)))
           (save-excursion
             (goto-char start)
             (insert "\n"))
           (pb-lisp/indent-parent-node t)
           (pb-lisp/select-current-node)))

       (defun pb-lisp/move-node-up-one-line ()
         "Move the current node or selection up one line."
         (interactive)
         (let* ((node (pb-lisp/get-current-node))
                (start (treesit-node-start node))
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
             (pb-lisp/indent-parent-node t)
             (pb-lisp/select-current-node)))))

(progn :evaluation

       (defvar pb-lisp/elisp-methods
         (km :eval
             (lambda (node-text)
               (interactive)
               (eval (read node-text)))

             :eval-pretty
             (lambda (node-text)
               (interactive)
               (pb-elisp_display-expression (eval (read node-text))))))

       (defvar pb-lisp/clojure-methods
         (km :eval
             (lambda (_)
               (interactive)
               (save-excursion
                 (goto-char (cdr pb-lisp/selection))
                 (cider-eval-last-sexp)))

             :eval-pretty
             (lambda (code-string)
               (interactive)
               (save-excursion
                 (goto-char (cdr pb-lisp/selection))
                 (cider-pprint-eval-last-sexp)))))

       (defvar pb-lisp/major-mode->methods
         `((emacs-lisp-mode ,@pb-lisp/elisp-methods)
           (clojure-mode ,@pb-lisp/clojure-methods)
           (clojurescript-mode ,@pb-lisp/clojure-methods)
           (clojurec-mode ,@pb-lisp/clojure-methods))
         "Maps tree-sitter language symbols to a plist of methods/configs.
         Each language entry contains:
         - :parser-lang - the language symbol for the tree-sitter parser
         - :get-node - the string name used when getting nodes with treesit-node-at
         - :modes - list of major modes associated with this language")

       (setq pb-lisp/major-mode->methods
             `((emacs-lisp-mode ,@pb-lisp/elisp-methods)
               (clojure-mode ,@pb-lisp/clojure-methods)
               (clojurescript-mode ,@pb-lisp/clojure-methods)
               (clojurec-mode ,@pb-lisp/clojure-methods)))

       (defun pb-lisp/get-method (k)
         (km_get (alist-get major-mode pb-lisp/major-mode->methods)
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

(quote
 (progn :improved-selection

  ;; Main state variables
  (defvar-local pb-lisp/current-node nil
    "The current primary node (cursor position)")

  (defvar-local pb-lisp/selection-count 0
    "Number of additional siblings selected after the current node")

  (defun pb-lisp/get-selected-nodes ()
    "Return list of currently selected nodes starting from current node."
    (print (list 'pb-lisp/get-selected-nodes
                 pb-lisp/selection-count))
    (let* ((node (pb-lisp/get-current-node))
           (parent (treesit-node-parent node))
           (idx (treesit-node-index node t))
           (child-count (treesit-node-child-count parent t))
           (end-idx (min (+ idx pb-lisp/selection-count) (1- child-count))))
      (print (kmq child-count
                  end-idx))
      (cons node
            (cl-loop for i from idx to end-idx
                     collect (treesit-node-child parent i t)))))

  (defun pb-lisp/update-overlay-from-nodes ()
    "Update overlay based on selected nodes."
    (pb-lisp/delete-overlay)
    (let* ((nodes (pb-lisp/get-selected-nodes))
           (start (treesit-node-start (car nodes)))
           (end (treesit-node-end (car (last nodes)))))
      (let ((overlay (make-overlay start end)))
        (overlay-put overlay 'face 'pb-lisp/overlay-face)
        (push overlay pb-lisp/overlays))))

  (defun pb-lisp/extend-selection-forward ()
    "Extend the current selection to include the next sibling node."
    (interactive)
    (let* ((node (pb-lisp/get-current-node))
           (parent (treesit-node-parent node))
           (idx (treesit-node-index node t))
           (child-count (treesit-node-child-count parent t)))
      (when (< (+ idx pb-lisp/selection-count) (1- child-count))
        (setq pb-lisp/selection-count (1+ pb-lisp/selection-count))
        (pb-lisp/update-overlay-from-nodes))))

  (defun pb-lisp/shrink-selection-forward ()
    "Shrink the current selection by removing the last node in the forward direction."
    (interactive)
    (when (> pb-lisp/selection-count 0)
      (setq pb-lisp/selection-count (1- pb-lisp/selection-count))
      (pb-lisp/update-overlay-from-nodes)))

  (defun pb-lisp/reset-selection ()
    "Reset to single node selection."
    (interactive)
    (setq pb-lisp/selection-count 0)
    (pb-lisp/update-overlay-from-nodes))))

(progn :bindings

       (+ 3 2)

       (defun pb-lisp/exit ()
         (interactive)
         (evil-pb-lisp-state -1)
         (evil-normal-state 1))

       (defvar pb-lisp/bindings
         (list (kbd "<escape>") #'pb-lisp/exit
               (kbd "<tab>") #'pb-lisp/indent-current-node
               (kbd "<return>") #'pb-lisp/move-node-down-one-line
               (kbd "S-<return>") #'pb-lisp/move-node-up-one-line
               "h" #'pb-lisp/goto-prev-sibling
               "l" #'pb-lisp/goto-next-sibling
               "j" #'pb-lisp/goto-first-child
               "k" #'pb-lisp/goto-parent
               (kbd "C-l") #'pb-lisp/goto-last-sibling
               (kbd "C-h") #'pb-lisp/goto-first-sibling

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
               "a" #'pb-lisp/insert-at-end
               "I" #'pb-lisp/insert-before
               "i" #'pb-lisp/insert-at-begining
               "e" #'pb-lisp/eval-current-node
               (kbd "C-e") #'pb-lisp/eval-pretty

               "t" #'pb-misc_toggle-hiding
               (kbd "C-t") #'hs-hide-level
               (kbd "q r") #'pb-lisp/gptel-request-replace
               "?" #'pb-lisp/log-node))

       (dolist (binding (sq_partition 2 2 pb-lisp/bindings))
         (evil-define-key* nil
           evil-pb-lisp-state-map
           (car binding)
           (cadr binding))))

'(a b (d
       (i (et) (et))
       c)
  d e)

(message "pb-lisp (treesit) loaded")

(progn :gptel-current-node

       (require 'pb-gptel)

       (defun pb-lisp/gptel-request-replace (&optional options)
         (interactive)
         (pb_let [(km_keys prompt callback) options]
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

(provide 'pb-lisp)
;;; pb-lisp.el ends here
