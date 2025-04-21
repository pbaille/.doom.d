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
(require 'symex-interface)

;; predicates

(defun pb-org/folded-p ()
  "Test if point is on a folded headline or plain list item."
  (and (or (org-at-heading-p)
           (org-at-item-p))
       (invisible-p (line-end-position))))

(defun pb-org/top-of-narrowed-subtree-p ()
  "Test if cursor is on the top heading of a narrowed buffer."
  (and (buffer-narrowed-p)
       (bobp)))

(defun pb-org/at-result-node-p ()
  "Test if cursor is on a result node."
  (save-excursion
    (beginning-of-line)
    (forward-char)
    (looking-at "+RESULTS:")))

(defun pb-org/first-heading-p ()
  "Test if the current heading is the first of its section."
  (and (org-at-heading-p)
       (let ((p (point)))
         (save-excursion
           (widen)
           (org-backward-heading-same-level 1)
           (= (point) p)))))

(defun pb-org/last-heading-p ()
  "Test if the current heading is the last of its section."
  (and (org-at-heading-p)
       (let ((p (point)))
         (save-excursion
           (widen)
           (org-forward-heading-same-level 1)
           (= (point) p)))))

(defun pb-org/semifolded-p ()
  "Test if current node is semifolded."
  (if-let ((lvl (pb-org/current-heading-level)))
      (save-excursion
        (org-next-visible-heading 1)
        (if-let ((nxt-lvl (pb-org/current-heading-level)))
            (and (< lvl nxt-lvl)
                 (pb-org/folded-p))))))

(defun pb-org/first-node-p ()
  "Test if current node is the first of its parent."
  (let* ((context (org-element-context))
         (parent (org-element-property :parent context))
         (node-beg (org-element-property :begin context))
         (parent-beg (org-element-property :contents-begin parent)))
    (if (= node-beg
           parent-beg)
        node-beg)))

(defun pb-org/last-node-p ()
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

(defun pb-org/current-heading-level ()
  "Get the level of current heading."
  (and (org-at-heading-p)
       (length (org-get-outline-path))))

(defun pb-org/node-type ()
  "Get the type of the current node."
  (car (org-element-context)))

(defun pb-org/node-bounds ()
  "Get the bound positions of current node."
  (let ((elem (org-element-context)))
    (cons (org-element-property :begin elem)
          (org-element-property :end elem))))

(defun pb-org/print-context ()
  "Prints infos on current node."
  (interactive)
  (pp (org-element-context)))

;; fold

(defun pb-org/semifold ()
  "Show one level of content."
  (interactive)
  (org-fold-hide-subtree)
  (org-cycle))

(defun pb-org/toggle-fold ()
  "Toggle narrowing of current subtree, folding it accordingly."
  (interactive)
  (if (pb-org/top-of-narrowed-subtree-p)
      (if (pb-org/semifolded-p)
          (org-fold-show-subtree)
        (pb-org/semifold))
    (cond ((pb-org/folded-p) (org-cycle))
          ((pb-org/semifolded-p) (org-fold-show-subtree))
          (t (org-fold-hide-subtree)))))

;; narrowing

(defun pb-org/widen ()
  "Fold and widen a narrowd subtree."
  (interactive)
  (org-fold-hide-subtree)
  (widen))

(defun pb-org/narrow ()
  "Narrow a subtree and semifold it."
  (interactive)
  (org-narrow-to-subtree)
  (pb-org/semifold))

(defun pb-org/widen-one-level ()
  "Widen one level, keeping the cursor on current heading."
  (let ((p (point)))
    (pb-org/widen)
    (org-up-element)
    (pb-org/narrow)
    (goto-char p)))

(defun pb-org/toggle-narrow ()
  "Toggle narrowing of current subtree, folding it accordingly."
  (interactive)
  (if (pb-org/top-of-narrowed-subtree-p)
      (pb-org/widen)
    (cond ((org-at-heading-p)
           (pb-org/narrow))
          ((or (org-at-block-p)
               (org-in-src-block-p))
           (org-edit-src-code)))))

;; moves

(defun pb-org/move (move)
  "Do MOVE taking care of narrowed subtree.
If buffer is narrowed, widen it before moving then narrow back."
  (interactive)
  (if (pb-org/top-of-narrowed-subtree-p)
      (progn (pb-org/widen)
             (funcall move)
             (pb-org/narrow))
    (funcall move)))

(defun pb-org/parent ()
  "Move to parent heading.
If buffer is narrowed, widen it and narrow the next node"
  (interactive)
  (let ((node-beg (car (pb-org/node-bounds))))
    (if (equal (point) node-beg)
        (pb-org/move #'org-up-element)
      (goto-char node-beg))))

(defun pb-org/previous-heading ()
  "Go to the previous visible heading."
  (interactive)
  (if (pb-org/top-of-narrowed-subtree-p)
      (pb-org/widen-one-level)
    (org-previous-visible-heading 1)))

(defun pb-org/down-element-safe ()
  "Wrap `org-down-element' because it can do weird things, like going backward."
  (let ((p (point)))
    (condition-case _
        (org-down-element)
      (error nil))
    (if (<= (point) p)
        (and (goto-char p)
             nil)
      (point))))

(defvar pb-org/lisp-flavors
  (list "clojure" "racket" "elisp" "emacs-lisp" "scheme" "fennel"))

(defun pb-org/at-code-block-p ()
  "Check if point is at the start of a Clojure src block in org mode."
  (when-let ((element (org-element-context)))
    (and
     (equal (org-element-type element) 'src-block)
     (equal (org-element-property :begin element) (point)))))

(defun pb-org/at-lisp-block-p ()
  "Check if point is at the start of a Clojure src block in org mode."
  (let ((element (org-element-context)))
    (and (eq (org-element-type element) 'src-block)
         (member (org-element-property :language element)
                 pb-org/lisp-flavors)
         (= (org-element-property :begin element) (point)))))

(defun pb-org/enter-lisp-block ()
  "Toggle symex mode if cursor is at the beginning of a clojure block."
  (interactive)
  (when (pb-org/at-lisp-block-p)
    (forward-line)
    (symex-enter-mode)
    (point)))

(defvar-local pb-org/enter-src-block-function nil)

(defun pb-org/down-element ()
  "Enter inside current element."
  (cond ((pb-org/at-lisp-block-p)
         (if pb-org/enter-src-block-function
             (funcall pb-org/enter-src-block-function)
           (pb-org/enter-lisp-block)))
        ((org-at-block-p)
         (org-edit-src-code))
        (t (pb-org/down-element-safe))))

(defun pb-org/walk-forward ()
  "Go to first child or to next node."
  (interactive)
  (or (pb-org/down-element-safe)
      (org-forward-element)))

(defun pb-org/move-down ()
  "Move down through visible nodes."
  (interactive)
  (if (pb-org/folded-p)
      (org-next-visible-heading 1)
    (pb-org/walk-forward)))

(defun pb-org/move-up ()
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

(defun pb-org/walk-backward ()
  "Go to first child or to next node."
  (interactive)
  (let ((p (point)))
    (pb-org/move-up)
    (when (pb-org/folded-p)
      (org-fold-show-subtree)
      (goto-char p)
      (pb-org/move-up))))

(defun pb-org/first-node ()
  "Goto first node."
  (interactive)
  (let* ((context (org-element-context))
         (parent (org-element-property :parent context)))
    (goto-char (org-element-property :contents-begin parent))))

(defun pb-org/up-to-level (n)
  "Bubble up through hierarchy until level N."
  (interactive)
  (let* ((context (org-element-context))
         (level (org-element-property :level context)))
    (when (or (not level) (< n level))
      (pb-org/parent)
      (pb-org/up-to-level n))))

(defun pb-org/last-node ()
  "Goto last node."
  (interactive)
  (let* ((context (org-element-context))
         (parent (org-element-property :parent context)))
    (goto-char (org-element-property :contents-end parent))
    (goto-char (car (pb-org/node-bounds)))
    (pb-org/up-to-level (1+ (org-element-property :level parent)))))

(defun pb-org/forward ()
  "Move forward at the same level.
If buffer is narrowed, widen it and narrow the next node"
  (interactive)
  (if (pb-org/top-of-narrowed-subtree-p)
      (progn (pb-org/widen)
             (org-forward-element)
             (pb-org/narrow))
    (unless (pb-org/last-node-p)
      (org-forward-element)
      (point))))

(defun pb-org/backward-same-level ()
  "Move backward at the same level."
  (interactive)
  (unless (pb-org/first-node-p)
    (let ((p (point)))
      (if (org-at-heading-p)
          (org-backward-heading-same-level 1))
      (if (equal (point) p)
          (progn (backward-char 1)
                 (goto-char (car (pb-org/node-bounds)))
                 (point))
        (point)))))

(defun pb-org/backward ()
  "Move backward at the same level.
If buffer is narrowed, widen it and narrow the previous node"
  (interactive)
  (if (pb-org/top-of-narrowed-subtree-p)
      (progn (pb-org/widen)
             (org-backward-element)
             (pb-org/narrow))
    (pb-org/backward-same-level)))

;; edit

(defun pb-org/delete ()
  "Delete current node, adding it to kill ring."
  (interactive)
  (let ((bounds (pb-org/node-bounds)))
    (kill-region (car bounds) (cdr bounds))))

(defun pb-org/copy ()
  "Copy current node."
  (interactive)
  (let ((bounds (pb-org/node-bounds)))
    (copy-region-as-kill (car bounds) (cdr bounds))))

(defun pb-org/paste-after ()
  "Paste after current node."
  (interactive)
  (goto-char (cdr (pb-org/node-bounds)))
  (yank))

(defun pb-org/paste-before ()
  "Paste before current node."
  (interactive)
  (goto-char (car (pb-org/node-bounds)))
  (yank))

(defun pb-org/move-subtree-up ()
  "Move current section up."
  (interactive)
  (if (org-at-heading-p)
      (org-move-subtree-up)
    (user-error "not on a heading")))

(defun pb-org/move-subtree-down ()
  "Move current section down."
  (interactive)
  (if (org-at-heading-p)
      (org-move-subtree-down)
    (user-error "not on a heading")))

(defun pb-org/insert-after ()
  "Enter insert mode after current node."
  (interactive)
  (goto-char (cdr (pb-org/node-bounds)))
  (insert "\n\n")
  (backward-char 2)
  (evil-insert-state))

(defun pb-org/insert-before ()
  "Enter insert mode before current node."
  (interactive)
  (goto-char (car (pb-org/node-bounds)))
  (insert "\n\n")
  (backward-char 2)
  (evil-insert-state))

(defun pb-org/insert-at-beginning ()
  "Enter insert mode at beginning of current node."
  (interactive)
  (goto-char (car (pb-org/node-bounds)))
  (evil-insert-state))

(defun pb-org/insert-at-end ()
  "Enter insert mode at end of current node."
  (interactive)
  (goto-char (cdr (pb-org/node-bounds)))
  (skip-chars-backward " \t\n")
  (evil-insert-state))

(defun pb-org/shift-one-line-down ()
  "Move the current down one line."
  (interactive)
  (goto-char (car (pb-org/node-bounds)))
  (insert "\n"))

(defun pb-org/shift-one-line-up ()
  "Move the current up one line."
  (interactive)
  (save-excursion
    (forward-line -1)
    (when (looking-at-p "^\\s-*$")
      (kill-whole-line))))

(defun pb-org/create-code-block ()
  "Create a code block after current node, prompting for language."
  (interactive)
  (let* ((langs (append pb-org/lisp-flavors
                        '("python" "shell" "sql" "js" "typescript"
                          "rust" "c" "c++" "java" "go"
                          "ruby" "html" "css" "plantuml" "dot"
                          "mermaid" "bash" "sh")))
         (lang (completing-read "Language: " langs)))
    (print lang)
    (pb-org/insert-after)
    (insert "#+begin_src " lang "\n")
    (save-excursion (insert "\n#+end_src\n"))
    (when (member lang pb-org/lisp-flavors)
      (insert "()")
      (when (fboundp 'symex-mode-interface)
        (symex-mode-interface)))))

(defun pb-org/get-current-code-block-mode ()
  "Get the major mode corresponding to the current source block language."
  (when (org-in-src-block-p)
    (let ((info (org-babel-get-src-block-info)))
      (org-src-get-lang-mode (nth 0 info)))))

(defun pb-org/symex-eval ()
  "Evaluate the current source block using the appropriate symex method.
This function retrieves the major mode corresponding to the current
source block language and then calls the symex evaluation method
appropriate for that language."
  (funcall (let ((major-mode (pb-org/get-current-code-block-mode)))
             (symex-interface-get-method :eval))))

(symex-interface-extend (list 'org-mode)
                        (list :eval #'pb-org/symex-eval))

(defun pb-org/eval-block ()
  "Eval code block."
  (interactive)
  (if (org-at-block-p)
      (call-interactively #'org-ctrl-c-ctrl-c)))

(defun pb-org/maybe-edit-block ()
  "Enter src edition if in a source block."
  (interactive)
  (when (org-in-block-p (list "src"))
    (org-edit-src-code)
    t))

(defun pb-org/click ()
  "Click mouse 1 action."
  (interactive)
  (or (pb-org/maybe-edit-block)
      (call-interactively #'evil-mouse-start-end)))

(defun pb-org/code-block-content-bounds ()
  "Get the bounds of the source block content at point.
Return a cons cell with (start . end) positions of the content."
  (when (org-at-block-p)
    (save-excursion
      (let ((element (org-element-at-point)))
        ;; (print (cadr element))
        (when (eq (org-element-type element) 'src-block)
          (let* ((value (org-element-property :value element)))
            (forward-line 1)
            (cons (point) (+ (point) (length value)))))))))

(defun pb-org/in-code-block-p ()
  "Return non-nil if point is inside a code block.
More specifically, return the language of the code block."
  (let ((element (org-element-at-point)))
    (and (eq (car element) 'src-block)
         (list (org-element-property :begin element)
               (org-element-property :end element)))))

(defun pb-org/code-block-goto-beg ()
  "If cursor is within a code block, goes back to the very beginning of it."
  (interactive)
  (when (org-in-src-block-p)
    (let ((element (org-element-at-point)))
      (when (eq (org-element-type element) 'src-block)
        (goto-char (org-element-property :begin element))))))

(defun pb-org/code-block-language ()
  "Return the language of the src block at point."
  (let ((element (org-element-at-point)))
    (when (eq (car element) 'src-block)
      (org-element-property :language element))))

(provide 'pb-org)
;;; pb-org.el ends here
