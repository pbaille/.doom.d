;;; pb-org.el --- org utils -*- lexical-binding: t; -*-

;; Author: Pierre Baille
;; URL: https://github.com/pbaille
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Org helpers.

;;; Description

;; Utility belt for manipulating org files
;; The initial motivation is to build a system for adding metadata to files and dirs

;; olp stands for outline path
;; this switches to the scratch org buffer and go to one of its sub header

;;; Code:

(require 'km)
(require 'org)
(require 'org-element)
(require 'evil)

;; predicates

(defun pb-org_folded-p ()
  "Test if point is on a folded headline or plain list item."
  (and (or (org-at-heading-p)
           (org-at-item-p))
       (invisible-p (line-end-position))))

(defun pb-org_top-of-narrowed-subtree-p ()
  "Test if cursor is on the top heading of a narrowed buffer."
  (and (buffer-narrowed-p)
       (bobp)))

(defun pb-org_at-result-node-p ()
  "Test if cursor is on a result node."
  (save-excursion
    (beginning-of-line)
    (forward-char)
    (looking-at "+RESULTS:")))

(defun pb-org_first-heading-p ()
  "Test if the current heading is the first of its section."
  (and (org-at-heading-p)
       (let ((p (point)))
         (save-excursion
           (widen)
           (org-backward-heading-same-level 1)
           (= (point) p)))))

(defun pb-org_last-heading-p ()
  "Test if the current heading is the last of its section."
  (and (org-at-heading-p)
       (let ((p (point)))
         (save-excursion
           (widen)
           (org-forward-heading-same-level 1)
           (= (point) p)))))

(defun pb-org_semifolded-p ()
  "Test if current node is semifolded."
  (if-let ((lvl (pb-org_current-heading-level)))
      (save-excursion
        (org-next-visible-heading 1)
        (if-let ((nxt-lvl (pb-org_current-heading-level)))
            (and (< lvl nxt-lvl)
                 (pb-org_folded-p))))))

(defun pb-org_first-node-p ()
  "Test if current node is the first of its parent."
  (let* ((context (org-element-context))
         (parent (org-element-property :parent context))
         (node-beg (org-element-property :begin context))
         (parent-beg (org-element-property :contents-begin parent)))
    (if (= node-beg
           parent-beg)
        node-beg)))

(defun pb-org_last-node-p ()
  "Test if current node is the last of its parent."
  (let* ((context (org-element-context))
         (parent (org-element-property :parent context))
         (parent (if (equal 'section (car parent))
                     (org-element-property :parent parent)
                   parent))
         (node-end (or (org-element-property :contents-end context)
                       (org-element-property :end context)))
         (parent-end (org-element-property :contents-end parent)))
    (if (= node-end
           parent-end)
        node-end)))

;; help

(defun pb-org_current-heading-level ()
  "Get the level of current heading."
  (and (org-at-heading-p)
       (length (org-get-outline-path))))

(defun pb-org_node-type ()
  "Get the type of the current node."
  (car (org-element-context)))

(defun pb-org_node-bounds ()
  "Get the bound positions of current node."
  (let ((elem (org-element-context)))
    (cons (org-element-property :begin elem)
          (org-element-property :end elem))))

(defun pb-org_print-context ()
  "Prints infos on current node."
  (interactive)
  (pp (org-element-context)))

;; fold

(defun pb-org_semifold ()
  "Show one level of content."
  (interactive)
  (org-fold-hide-subtree)
  (org-cycle))

(defun pb-org_toggle-fold ()
  "Toggle narrowing of current subtree, folding it accordingly."
  (interactive)
  (if (pb-org_top-of-narrowed-subtree-p)
      (if (pb-org_semifolded-p)
          (org-fold-show-subtree)
        (pb-org_semifold))
    (cond ((pb-org_folded-p) (org-cycle))
          ((pb-org_semifolded-p) (org-fold-show-subtree))
          (t (org-fold-hide-subtree)))))

;; narrowing

(defun pb-org_widen ()
  "Fold and widen a narrowd subtree."
  (interactive)
  (org-fold-hide-subtree)
  (widen))

(defun pb-org_narrow ()
  "Narrow a subtree and semifold it."
  (interactive)
  (org-narrow-to-subtree)
  (pb-org_semifold))

(defun pb-org_widen-one-level ()
  "Widen one level, keeping the cursor on current heading."
  (let ((p (point)))
    (pb-org_widen)
    (org-up-element)
    (pb-org_narrow)
    (goto-char p)))

(defun pb-org_toggle-narrow ()
  "Toggle narrowing of current subtree, folding it accordingly."
  (interactive)
  (if (pb-org_top-of-narrowed-subtree-p)
      (pb-org_widen)
    (cond ((org-at-heading-p)
           (pb-org_narrow))
          ((or (org-at-block-p)
               (org-in-src-block-p))
           (org-edit-src-code)))))

;; moves

(defun pb-org_move (move)
  "Do MOVE taking care of narrowed subtree.
If buffer is narrowed, widen it before moving then narrow back."
  (interactive)
  (if (pb-org_top-of-narrowed-subtree-p)
      (progn (pb-org_widen)
             (funcall move)
             (pb-org_narrow))
    (funcall move)))

(defun pb-org_parent ()
  "Move to parent heading.
If buffer is narrowed, widen it and narrow the next node"
  (interactive)
  (let ((node-beg (car (pb-org_node-bounds))))
    (if (equal (point) node-beg)
        (pb-org_move #'org-up-element)
      (goto-char node-beg))))

(defun pb-org_previous-heading ()
  "Go to the previous visible heading."
  (interactive)
  (if (pb-org_top-of-narrowed-subtree-p)
      (pb-org_widen-one-level)
    (org-previous-visible-heading 1)))

(defun pb-org_down-element-safe ()
  "Wrap `org-down-element' because it can do weird things, like going backward."
  (let ((p (point)))
    (condition-case _
        (org-down-element)
      (error nil))
    (if (<= (point) p)
        (and (goto-char p)
             nil)
      (point))))

(defun pb-org_down-element ()
  "Enter inside current element."
  (if (org-at-block-p)
      (org-edit-src-code)
    (pb-org_down-element-safe)))

(defun pb-org_walk-forward ()
  "Go to first child or to next node."
  (interactive)
  (or (pb-org_down-element-safe)
      (org-forward-element)))

(defun pb-org_move-down ()
  "Move down through visible nodes."
  (interactive)
  (if (pb-org_folded-p)
      (org-next-visible-heading 1)
    (pb-org_walk-forward)))

(defun pb-org_move-up ()
  "Move up through visible nodes."
  (interactive)
  (let ((p (point)))
    (cond ((org-at-heading-p)
           (evil-previous-line)
           (if (org--line-empty-p 1)
               (org-backward-element)))
          (t (org-backward-element)))
    (if (= p (point))
        (org-previous-visible-heading 1))))

(defun pb-org_walk-backward ()
  "Go to first child or to next node."
  (interactive)
  (let ((p (point)))
    (pb-org_move-up)
    (when (pb-org_folded-p)
      (org-fold-show-subtree)
      (goto-char p)
      (pb-org_move-up))))

(defun pb-org_first-node ()
  "Goto first node."
  (interactive)
  (let* ((context (org-element-context))
         (parent (org-element-property :parent context)))
    (goto-char (org-element-property :contents-begin parent))))

(defun pb-org_up-to-level (n)
  "Bubble up through hierarchy until level N."
  (interactive)
  (let* ((context (org-element-context))
         (level (org-element-property :level context)))
    (when (or (not level) (< n level))
      (pb-org_parent)
      (pb-org_up-to-level n))))

(defun pb-org_last-node ()
  "Goto last node."
  (interactive)
  (let* ((context (org-element-context))
         (parent (org-element-property :parent context)))
    (goto-char (org-element-property :contents-end parent))
    (goto-char (car (pb-org_node-bounds)))
    (pb-org_up-to-level (1+ (org-element-property :level parent)))))

(defun pb-org_forward ()
  "Move forward at the same level.
If buffer is narrowed, widen it and narrow the next node"
  (interactive)
  (if (pb-org_top-of-narrowed-subtree-p)
      (progn (pb-org_widen)
             (org-forward-element)
             (pb-org_narrow))
    (unless (pb-org_last-node-p)
      (org-forward-element))))

(defun pb-org_backward-same-level ()
  "Move backward at the same level."
  (interactive)
  (unless (pb-org_first-node-p)
    (let ((p (point)))
      (if (org-at-heading-p)
          (org-backward-heading-same-level 1))
      (if (equal (point) p)
          (progn (backward-char 1)
                 (goto-char (car (pb-org_node-bounds)))
                 (point))
        (point)))))

(defun pb-org_backward ()
  "Move backward at the same level.
If buffer is narrowed, widen it and narrow the previous node"
  (interactive)
  (if (pb-org_top-of-narrowed-subtree-p)
      (progn (pb-org_widen)
             (org-backward-element)
             (pb-org_narrow))
    (pb-org_backward-same-level)))

;; edit

(defun pb-org_delete ()
  "Delete current node, adding it to kill ring."
  (interactive)
  (let ((bounds (pb-org_node-bounds)))
    (kill-region (car bounds) (cdr bounds))))

(defun pb-org_copy ()
  "Copy current node."
  (interactive)
  (let ((bounds (pb-org_node-bounds)))
    (copy-region-as-kill (car bounds) (cdr bounds))))

(defun pb-org_paste-after ()
  "Paste after current node."
  (interactive)
  (goto-char (cdr (pb-org_node-bounds)))
  (yank))

(defun pb-org_paste-before ()
  "Paste before current node."
  (interactive)
  (goto-char (car (pb-org_node-bounds)))
  (yank))

(defun pb-org_move-subtree-up ()
  "Move current section up."
  (interactive)
  (if (org-at-heading-p)
      (org-move-subtree-up)
    (user-error "not on a heading")))

(defun pb-org_move-subtree-down ()
  "Move current section down."
  (interactive)
  (if (org-at-heading-p)
      (org-move-subtree-down)
    (user-error "not on a heading")))

(defun pb-org_insert-after ()
  "Enter insert mode after current node."
  (interactive)
  (goto-char (cdr (pb-org_node-bounds)))
  (insert "\n\n")
  (backward-char 2)
  (evil-insert-state))

(defun pb-org_insert-before ()
  "Enter insert mode before current node."
  (interactive)
  (goto-char (car (pb-org_node-bounds)))
  (insert "\n\n")
  (backward-char 2)
  (evil-insert-state))

(defun pb-org_insert-at-beginning ()
  "Enter insert mode at beginning of current node."
  (interactive)
  (goto-char (car (pb-org_node-bounds)))
  (evil-insert-state))

(defun pb-org_insert-at-end ()
  "Enter insert mode at end of current node."
  (interactive)
  (goto-char (cdr (pb-org_node-bounds)))
  (skip-chars-backward " \t\n")
  (evil-insert-state))

(defun pb-org_shift-one-line-down ()
  "Move the current down one line."
  (interactive)
  (goto-char (car (pb-org_node-bounds)))
  (insert "\n"))

(defun pb-org_shift-one-line-up ()
  "Move the current up one line."
  (interactive)
  (save-excursion
    (forward-line -1)
    (when (looking-at-p "^\\s-*$")
      (kill-whole-line))))

(defun pb-org_create-code-block ()
  "Create a code block after current node."
  (interactive)
  (pb-org_insert-after)
  (insert "#+begin_src ")
  (let ((p (point)))
    (insert "\n\n#+end_src\n")
    (goto-char p)))

(defun pb-org_eval-block ()
  "Eval code block."
  (interactive)
  (if (org-at-block-p)
      (call-interactively #'org-ctrl-c-ctrl-c)))

(defun pb-org_maybe-edit-block ()
  "Enter src edition if in a source block."
  (interactive)
  (when (org-in-block-p (list "src"))
    (org-edit-src-code)
    t))

(defun pb-org_click ()
  "Click mouse 1 action."
  (interactive)
  (or (pb-org_maybe-edit-block)
      (call-interactively #'evil-mouse-start-end)))

(provide 'pb-org)
;;; pb-org.el ends here.
