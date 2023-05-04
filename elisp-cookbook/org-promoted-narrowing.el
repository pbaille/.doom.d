;;; elisp-cookbook/org-promoted-narrowing.el -*- lexical-binding: t; -*-

(defun my-org-narrow-to-subtree-and-promote ()
  "Narrow to the current subtree and promote it to a top-level heading.
   Store the original level of the heading in a buffer-local variable."
  (interactive)
  (if (buffer-narrowed-p)
      (my-org-widen-and-demote))
  (org-narrow-to-subtree)
  (org-set-property "ORIGINAL_HEADING_LVL" (number-to-string (org-current-level)))
  (while (> (org-current-level) 1)
    (org-promote-subtree))
  (outline-show-all))

(defun my-org-widen-and-demote ()
  "Widen the buffer and demote the heading to its original level."
  (interactive)
  (if (buffer-narrowed-p)
      (save-excursion
        (goto-char (point-min))
        (let ((original-level (string-to-number (org-entry-get (point-min) "ORIGINAL_HEADING_LVL"))))
          (while (> original-level (org-current-level))
            (org-demote-subtree)))
        (org-delete-property "ORIGINAL_HEADING_LVL")))
  (widen))
