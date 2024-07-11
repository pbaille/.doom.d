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

(defun pb-org_folded-p ()
  "Test if point is on a folded headline or plain list item."
  (and (or (org-at-heading-p)
           (org-at-item-p))
       (invisible-p (point-at-eol))))

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

(defun pb-org_semifold ()
  "Show one level of content."
  (interactive)
  (org-fold-hide-subtree)
  (org-cycle))

(defun pb-org_current-heading-level ()
  "Get the level of current heading."
  (and (org-at-heading-p)
       (length (org-get-outline-path))))

(defun pb-org_semifolded-p ()
  "Test if current node is semifolded."
  (let ((lvl (pb-org_current-heading-level)))
    (save-excursion
     (org-next-visible-heading 1)
     (let ((nxt-lvl (pb-org_current-heading-level)))
       (and (< lvl nxt-lvl)
            (pb-org_folded-p))))))

(defun pb-org_widen ()
  "Fold and widen a narrowd subtree."
  (interactive)
  (org-fold-hide-subtree)
  (widen)
  (recenter))

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

(defun pb-org_go-forward ()
  "Depending on cursor position, enter a node, edit src code or move forward.

- cursor is on a narrowed buffer top header:

  try to move to the next section of same level, narrowing it.
  if not possible (last heading), widen one level up.

- cursor is on a folded heading:

  narrow the buffer to it.

- cursor is on code block:

  enter edit mode.

- in other cases:

  forward one word."
  (interactive)
  (cond ((pb-org_top-of-narrowed-subtree-p)
         (if (pb-org_last-heading-p)
             (pb-org_widen-one-level)
           (progn (pb-org_widen)
                 (org-forward-heading-same-level 1)
                 (pb-org_narrow))))
        ((and (org-at-heading-p)
              (pb-org_folded-p))
         (pb-org_narrow))
        ((org-at-block-p)
         (org-edit-src-code))
        (t (forward-word))))

(defun pb-org_go-backward ()
  "Depending on cursor position, exit a node, exit code edition or move back.

- cursor is on a narrowed buffer top header:

  try to move to the previous section of same level, narrowing it.
  if not possible (first heading), widen one level up.

- cursor is on a heading:

  go to parent heading.

- from the first char of an org src edition buffer:

  exit edit mode.

- in other cases:

  backward one word."
  (interactive)
  (cond (org-src-mode
         (if (bobp)
             (org-edit-src-exit)
           (call-interactively #'evil-backward-char)))
        ((pb-org_top-of-narrowed-subtree-p)
         (if (pb-org_first-heading-p)
             (pb-org_widen-one-level)
           (progn (pb-org_widen)
                  (org-backward-heading-same-level 1)
                  (pb-org_narrow))))
        ((org-at-heading-p)
         (org-up-element))
        (t (backward-word))))

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
  (pb-org_move #'org-up-element))

(defun pb-org_down-element ()
  "Wrap `org-down-element' because it can do weird things, like going backward."
  (let ((p (point)))
    (condition-case err
        (org-down-element)
      (error (org-forward-element)))
    (if (< (point) p)
        (and (goto-char p)
             nil)
      (point))))

(defun pb-org_walk-forward ()
  "Go to first child or to next node."
  (interactive)
  (or (pb-org_down-element)
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

(defun pb-org_forward ()
  "Move forward at the same level.
If buffer is narrowed, widen it and narrow the next node"
  (interactive)
  (if (pb-org_top-of-narrowed-subtree-p)
      (progn (pb-org_widen)
             (org-forward-element)
             (pb-org_narrow))
    (org-forward-element)))

(defun pb-org_backward ()
  "Move backward at the same level.
If buffer is narrowed, widen it and narrow the previous node"
  (interactive)
  (if (pb-org_top-of-narrowed-subtree-p)
      (progn (pb-org_widen)
             (org-backward-element)
             (pb-org_narrow))
    (org-backward-element)))

(defun pb-org_cut ()
  "Cut current section. if cursor on heading."
  (interactive)
  (cond ((org-at-heading-p)
         (org-cut-subtree))
        ((or (org-at-block-p)
             (pb-org_at-result-node-p))
         (kill-region (point) (save-excursion (pb-org_move-down) (point))))
        (t (call-interactively #'evil-delete))))

(defun pb-org_copy ()
  "Copy current section. if cursor on heading."
  (interactive)
  (if (org-at-heading-p)
      (org-copy-subtree)
    (call-interactively #'evil-yank)))

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
















(list :not-needed
      ;; org subtree actions: cut paste etc...
      (defun pb-org_beginning-of-section ()
        "Go to beggining of current section."
        (interactive)
        (unless (org-at-heading-p)
          (org-previous-visible-heading 1))
        (beginning-of-line))

      (defun pb-org_end-of-section ()
        "Go to end of current section."
        (interactive)
        (let ((lvl (save-excursion (pb-org_beginning-of-section)
                                   (pb-org_current-heading-level)))
              (in t))
          (while in
            (org-next-visible-heading 1)
            (when (or (<= (pb-org_current-heading-level)
                          lvl)
                      (eobp))
              (setq in nil)
              (beginning-of-line)))))

      (defun pb-org_section-at-point ()
        (cons (save-excursion (pb-org_beginning-of-section) (point))
              (save-excursion (pb-org_end-of-section) (point))))

      (defun pb-org_kill-current-section ()
        (interactive)
        (pb_let [(cons beg end) (pb-org_section-at-point)]
            (kill-region beg end)))

      (defun pb-org_thing-at-point ()
        (save-excursion
          (cond ((org-at-heading-p)
                 (beginning-of-line)
                 (let ((beg (point)))
                   (org-forward-heading-same-level)
                   (if (> (point) beg)
                       (cons beg (progn (beginning-of-line) (point)))
                     )))
                ((org-at-block-p) ())
                (t ())))))

(progn :org-xp

       '(progn (switch-to-buffer "scratch.org")
               (goto-char (org-find-olp (list "~/org/scratch.org" "top" "three" "3.2"))))

       (km_defun pb-org_find-or-create-olp-aux
                 (:as opts
                      file current-path remaining-path)
                 (let* ((next-path (append current-path (list (car remaining-path))))
                        (next-marker (ignore-errors (org-find-olp (cons file next-path)))))
                   (if next-marker
                       (let* ((remaining-next (cdr remaining-path))
                              (next-opts (km_put opts
                                                 :marker next-marker
                                                 :current-path next-path
                                                 :remaining-path remaining-next)))
                         (if remaining-next
                             (pb-org_find-or-create-olp-aux next-opts)
                           next-opts))
                     opts)))

       (defun pb-org_find-or-create-olp (file path)
         "Find or create outline path PATH in FILE."
         (km_let ((remaining-path marker)
                  (pb-org_find-or-create-olp-aux
                   (km :file file
                       :marker 0
                       :current-path ()
                       :remaining-path path)))

                 (switch-to-buffer (get-file-buffer file))
                 (goto-char marker)
                 (cl-loop for i in remaining-path
                          do (org-insert-subheading 0) (insert i))))

       (defun pb-org_put (file path content)
         "Put CONTENT at PATH in FILE."
         (pb-org_find-or-create-olp file path)
         (cond ((stringp content) (evil-open-below 1) (insert content))
               ((km? content)
                (org-set-tags (km_get content :tags))
                (evil-open-below 1) (insert (km_get content :text)))))

       (defvar pb-org_file-infos "~/org/file-infos.org"
         "The main org file to hold file infos.")

       (defun pb-org_insert-file-info ()
         "Return the capture target for file info."
         (interactive)
         (pb-org_find-or-create-olp pb-org_file-infos
                                    (split-string (buffer-file-name) "/" t))
         (org-narrow-to-subtree)
         (org-end-of-subtree))

       '(:org-put-tries
         (pb-org_find-or-create-olp "~/org/scratch.org" (list "top" "tao"))
         (pb-org_find-or-create-olp "~/org/scratch.org" (list "top" "tao" "baz" "iop"))
         (pb-org_put "~/org/scratch.org" (list "top" "tao" "baz" "iop")
                     "hello you")
         (pb-org_put "~/org/scratch.org" (list "top" "tao" "baz" "iop")
                     (km :tags (list "pouet" "foo")
                         :text "very interesting indeed")))

       '(:capture-hook-xp
         (defun pb-after-capture-action ()
           (print (file-name-parent-directory (org-capture-get :original-file)))
           (org-refile nil nil (list "a" "file-infos.org" nil nil)))

         (add-hook 'org-capture-before-finalize-hook
                   #'pb-after-capture-action)
         (remove-hook 'org-capture-before-finalize-hook
                      #'pb-after-capture-action))

       (provide 'pb-org)
;;; pb-org.el ends here.
       )
