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
(require 'tsc)
(require 'tree-sitter)
(require 'tree-sitter-langs)

(progn :mode-definition
       (evil-define-state pb-lisp
         "Sorg state."
         :tag " PB_LISP "
         :message "-- PB_LISP --"
         :enable (normal)
         :entry-hook (pb-lisp/enter-mode)
         :exit-hook (pb-lisp/exit-mode))

       (setq evil-pb-lisp-state-cursor
             `(box "magenta"))

       (defvar pb-lisp/modes
         '(clojure-mode clojurescript-mode clojurec-mode
           emacs-lisp-mode fennel-mode sheme-mode racket-mode))

       (defun pb-lisp/enter-mode ()
         "Run when on entering sorg mode."
         (when (member major-mode pb-lisp/modes)
           (tree-sitter-mode)
           (goto-char (car (pb-lisp/get-current-node-bounds)))
           (pb-lisp/update-overlay)))

       (defun pb-lisp/exit-mode ()
         "Run on exiting sorg mode."
         (print "exit pb-lisp")
         (pb-lisp/delete-overlay)
         (tree-sitter-mode -1)))

(progn :overlay
       (defface pb-lisp/current-node-face
         '((t :inherit symex--current-node-face :extend nil))
         "Face used to highlight the current tree node."
         :group 'pb-lisp/faces)

       (defvar pb-lisp/current-overlay nil "The current overlay which highlights the current node.")

       (defun pb-lisp/delete-overlay ()
         "Delete the highlight overlay."
         (when pb-lisp/current-overlay
           (delete-overlay pb-lisp/current-overlay)))

       (defun pb-lisp/update-overlay (&optional bounds)
         "Update the highlight overlay to match the start/end position of NODE."
         (interactive)
         (pb-lisp/delete-overlay)
         (setq-local pb-lisp/current-overlay
                     (let ((bounds (or bounds (pb-lisp/get-current-node-bounds))))
                       (make-overlay (car bounds) (cdr bounds))))
         (overlay-put pb-lisp/current-overlay 'face 'pb-lisp/current-node-face)))

(progn :bindings

       (defun pb-lisp/exit ()
         (interactive)
         (evil-pb-lisp-state -1))

       (defvar pb-lisp/bindings
         (list "ESC" #'pb-lisp/exit
               "h" #'pb-lisp/goto-prev-sibling
               "l" #'pb-lisp/goto-next-sibling
               "j" #'pb-lisp/goto-first-child
               "k" #'pb-lisp/goto-parent
               "C-l" #'pb-lisp/goto-last-sibling
               "C-h" #'pb-lisp/goto-first-sibling
               "L" #'pb-lisp/extend-selection-to-next-sibling
               "H" #'pb-lisp/extend-selection-to-prev-sibling
               "J" #'pb-lisp/shrink-selection-from-beg
               "K" #'pb-lisp/shrink-selection-from-end
               ;; should use alt instead of ctrl
               "C-L" #'pb-lisp/swap-with-next-sibling
               "C-H" #'pb-lisp/swap-with-prev-sibling
               ))

       (dolist (binding (sq_partition 2 2 pb-lisp/bindings))
         (define-key evil-pb-lisp-state-map
                     (kbd (car binding))
                     (cadr binding))

         (advice-remove (cadr binding) #'pb-lisp/update-overlay)))

(progn :current-node

       ;; symex-ts--get-topmost-node
       (defun pb-lisp/get-topmost-node (node)
         "Return the highest node in the tree starting from NODE.

The returned node is the highest possible node that has the same
start position as NODE."
         (let ((node-start-pos (tsc-node-start-position node))
               (parent (tsc-get-parent node)))
           (if parent
               (let ((parent-pos (tsc-node-start-position parent)))
                 (if (eq node-start-pos parent-pos)
                     (pb-lisp/get-topmost-node parent)
                   node))
             node)))

       (defun pb-lisp/get-current-node ()
         "Get the tree-sitter node at point."
         (if tree-sitter-tree
             (let* ((root (tsc-root-node tree-sitter-tree))
                    (pos (point)))
               (pb-lisp/get-topmost-node
                (tsc-get-named-descendant-for-position-range root pos pos)))
           (message "tree-sitter not enabled")))

       (defun pb-lisp/get-current-node-bounds ()
         "Get the start and end positions of the current tree-sitter node.
Returns a cons cell (start . end) with buffer positions."
         (tsc-node-position-range (pb-lisp/get-current-node))))

(progn :motion
       (defun pb-lisp/goto-node (node message)
         "Go to the start position of NODE or display MESSAGE if node is nil."
         (if node
             (progn (goto-char (tsc-node-start-position node))
                    (pb-lisp/update-overlay (tsc-node-position-range node)))
           (message message)))

       (defun pb-lisp/goto-parent ()
         "Move to the beginning of the parent node."
         (interactive)
         (let* ((node (pb-lisp/get-current-node))
                (parent (tsc-get-parent node)))
           (pb-lisp/goto-node parent "No parent node found")))

       (defun pb-lisp/get-node-child-index (node parent)
         "Get the index of NODE among the named children of PARENT.
Returns nil if NODE is not a child of PARENT."
         (when (and node parent)
           (let ((child-count (tsc-count-named-children parent)))
             (cl-loop for i from 0 below child-count
                      when (tsc-node-eq node (tsc-get-nth-named-child parent i))
                      return i))))

       (defun pb-lisp/goto-next-sibling ()
         "Move to the next sibling node."
         (interactive)
         (let* ((node (pb-lisp/get-current-node))
                (parent (tsc-get-parent node))
                (child-count (and parent (tsc-count-named-children parent)))
                (current-index (pb-lisp/get-node-child-index node parent))
                (is-last-sibling (and current-index (= current-index (1- child-count))))
                (next-sibling (and parent (not is-last-sibling) (tsc-get-next-sibling node))))
           (message "Debug:\nnode=%S\nparent=%S\nchild-count=%S\ncurrent-index=%S\nis-last-sibling=%S\nnext-sibling=%S"
                    (tsc-node-type node)
                    (and parent (tsc-node-type parent))
                    child-count
                    current-index
                    is-last-sibling
                    (and next-sibling (tsc-node-type next-sibling)))
           (pb-lisp/goto-node next-sibling "No next sibling found")))

       (defun pb-lisp/goto-prev-sibling ()
         "Move to the previous sibling node."
         (interactive)
         (let* ((node (pb-lisp/get-current-node))
                (parent (tsc-get-parent node))
                (current-index (pb-lisp/get-node-child-index node parent))
                (is-first-sibling (and current-index (= current-index 0)))
                (prev-sibling (and parent (not is-first-sibling) (tsc-get-prev-sibling node))))
           (pb-lisp/goto-node prev-sibling "No previous sibling found")))

       (defun pb-lisp/goto-first-sibling ()
         "Move to the first sibling node (first child of parent)."
         (interactive)
         (let* ((node (pb-lisp/get-current-node))
                (parent (tsc-get-parent node))
                (first-sibling (and parent (tsc-get-nth-named-child parent 0))))
           (pb-lisp/goto-node first-sibling "No first sibling found")))

       (defun pb-lisp/goto-last-sibling ()
         "Move to the last sibling node (last child of parent)."
         (interactive)
         (let* ((node (pb-lisp/get-current-node))
                (parent (tsc-get-parent node))
                (child-count (and parent (tsc-count-named-children parent)))
                (last-sibling (and child-count (> child-count 0)
                                   (tsc-get-nth-named-child parent (1- child-count)))))
           (pb-lisp/goto-node last-sibling "No last sibling found")))

       (defun pb-lisp/goto-first-child ()
         "Move to the first child of current node."
         (interactive)
         (let* ((node (pb-lisp/get-current-node))
                (child (and node (tsc-get-nth-named-child node 0))))
           (pb-lisp/goto-node child "No child node found")))

       (defun pb-lisp/goto-last-child ()
         "Move to the last child of current node."
         (interactive)
         (let* ((node (pb-lisp/get-current-node))
                (count (tsc-count-children node))
                (child (and (> count 0) (tsc-get-nth-child node (1- count)))))
           (pb-lisp/goto-node child "No child node found")))
       )

(progn :selection
       (defun pb-lisp/extend-selection-to-next-sibling ()
         "Extend the current selection to include the next sibling node."
         (interactive)
         (let* ((start (overlay-start pb-lisp/current-overlay))
                ;; Go to the end position of the overlay before finding next sibling
                (_ (goto-char (1- (overlay-end pb-lisp/current-overlay))))
                (node (pb-lisp/get-current-node))
                (next-sibling (and (tsc-get-parent node) (tsc-get-next-sibling node))))
           (goto-char start)
           (if next-sibling
               (pb-lisp/update-overlay (cons start (tsc-node-end-position next-sibling)))
             (message "No next sibling found"))))

       (defun pb-lisp/extend-selection-to-prev-sibling ()
         "Extend the current selection to include the previous sibling node."
         (interactive)
         (let* ((end (overlay-end pb-lisp/current-overlay))
                ;; Go to the start position of the overlay before finding prev sibling
                (_ (goto-char (overlay-start pb-lisp/current-overlay)))
                (node (pb-lisp/get-current-node))
                (prev-sibling (and (tsc-get-parent node) (tsc-get-prev-sibling node))))
           (if prev-sibling
               (let ((start (tsc-node-start-position prev-sibling)))
                 (goto-char start)
                 (pb-lisp/update-overlay (cons start end)))
             (message "No previous sibling found"))))

       (defun pb-lisp/get-selected-nodes ()
         "Get all nodes covered by the current selection overlay.
Returns a list of (parent child-count node-list selected-indices) where:
- parent is the parent node
- child-count is the number of children
- node-list is the list of named children
- selected-indices is a cons of (first-selected-index . last-selected-index)"
         (let* ((start (overlay-start pb-lisp/current-overlay))
                (end (overlay-end pb-lisp/current-overlay))
                (node (pb-lisp/get-current-node))
                (parent (tsc-get-parent node))
                (child-count (and parent (tsc-count-named-children parent)))
                nodes selected-nodes first-idx last-idx)

           (when (and parent child-count (> child-count 0))
             ;; Get all child nodes
             (setq nodes (cl-loop for i from 0 below child-count
                                  collect (tsc-get-nth-named-child parent i)))

             ;; Find which nodes are in our selection
             (setq selected-nodes
                   (cl-loop for node in nodes
                            for i from 0
                            for node-start = (tsc-node-start-position node)
                            for node-end = (tsc-node-end-position node)
                            when (and (<= start node-end) (>= end node-start))
                            collect (cons i node)))

             (when selected-nodes
               (setq first-idx (caar selected-nodes)
                     last-idx (caar (last selected-nodes)))

               ;; Print information about selected nodes
               (cl-loop for (idx . node) in selected-nodes
                        for text = (buffer-substring-no-properties
                                    (tsc-node-start-position node)
                                    (tsc-node-end-position node))
                        do (message "  Node %d: %s [%s]"
                                    idx
                                    (tsc-node-type node)
                                    (if (> (length text) 30)
                                        (concat (substring text 0 27) "...")
                                      text)))

               (list parent child-count nodes (cons first-idx last-idx))))))



       (defun pb-lisp/shrink-selection-from-end ()
         "Shrink the current selection by excluding the last sibling."
         (interactive)
         (let* ((selection-data (pb-lisp/get-selected-nodes))
                (start (overlay-start pb-lisp/current-overlay)))
           (if (and selection-data
                    (> (cdr (nth 3 selection-data)) (car (nth 3 selection-data))))
               (let* ((nodes (nth 2 selection-data))
                      (indices (nth 3 selection-data))
                      (new-last-idx (1- (cdr indices)))
                      (new-last-node (nth new-last-idx nodes))
                      (new-end (tsc-node-end-position new-last-node)))
                 (pb-lisp/update-overlay (cons start new-end)))
             (message "Cannot shrink selection further"))))

       (defun pb-lisp/shrink-selection-from-beg ()
         "Shrink the current selection by excluding the first sibling."
         (interactive)
         (let* ((selection-data (pb-lisp/get-selected-nodes))
                (end (overlay-end pb-lisp/current-overlay)))
           (if (and selection-data
                    (< (car (nth 3 selection-data)) (cdr (nth 3 selection-data))))
               (let* ((nodes (nth 2 selection-data))
                      (indices (nth 3 selection-data))
                      (new-first-idx (1+ (car indices)))
                      (new-first-node (nth new-first-idx nodes))
                      (new-start (tsc-node-start-position new-first-node)))
                 (goto-char new-start)
                 (pb-lisp/update-overlay (cons new-start end)))
             (message "Cannot shrink selection further"))))


       )

(progn :move-expressions

       (defun pb-lisp/swap-siblings (direction)
         "Transpose the current node with its next or previous sibling.
DIRECTION should be 'next or 'prev."
         (let* ((node (pb-lisp/get-current-node))
                (parent (tsc-get-parent node))
                (sibling (cond ((eq direction 'next) (tsc-get-next-sibling node))
                               ((eq direction 'prev) (tsc-get-prev-sibling node))
                               (t nil)))
                (node-start (tsc-node-start-position node))
                (node-end (tsc-node-end-position node))
                (node-text (buffer-substring-no-properties node-start node-end))
                (sibling-start (and sibling (tsc-node-start-position sibling)))
                (sibling-end (and sibling (tsc-node-end-position sibling)))
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

       (defun pb-lisp/swap-with-next-sibling ()
         "Swap the current node with its next sibling."
         (interactive)
         (pb-lisp/swap-siblings 'next))

       (defun pb-lisp/swap-with-prev-sibling ()
         "Swap the current node with its previous sibling."
         (interactive)
         (pb-lisp/swap-siblings 'prev)))

'(a b c d e)

(map! (:map emacs-lisp-mode-map
       :n "s-l" #'evil-pb-lisp-state)
      (:map clojure-mode-map
       :n "M-`" #'evil-pb-lisp-state))

(provide 'pb-lisp)
;;; pb-lisp.el ends here
