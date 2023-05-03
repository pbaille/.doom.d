;;; elisp-cookbook/org-promoted-narrowing.el -*- lexical-binding: t; -*-

(setq my-org-original-level 1)

(defun my-org-narrow-to-subtree-and-promote ()
  "Narrow to the current subtree and promote it to a top-level heading.
   Store the original level of the heading in a buffer-local variable."
  (interactive)
  (org-narrow-to-subtree)
  (setq-local my-org-original-level (org-current-level))
  (while (> (org-current-level) 1)
    (org-promote-subtree))
  (outline-show-all))

(defun my-org-widen-and-demote ()
  "Widen the buffer and demote the heading to its original level."
  (interactive)
  (while (> my-org-original-level (org-current-level))
    (org-demote-subtree))
  (widen))
