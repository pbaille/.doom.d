;;; elisp-cookbook/org-promoted-narrowing.el -*- lexical-binding: t; -*-

(defconst my-org-promoted-narrowing-lvl-property "ORIGINAL_HEADING_LVL")

(defun my-org-promoted-narrow ()
  "Narrow to the current subtree and promote it to a top-level heading.
   Store the original level of the heading in a buffer-local variable."
  (interactive)
  (if (buffer-narrowed-p)
      (my-org-demoted-widen))
  (org-narrow-to-subtree)
  (org-set-property my-org-promoted-narrowing-lvl-property (number-to-string (org-current-level)))
  (while (> (org-current-level) 1)
    (org-promote-subtree))
  (outline-show-all))

(defun my-org-demoted-widen ()
  "Widen the buffer and demote the heading to its original level."
  (interactive)
  (if (buffer-narrowed-p)
      (save-excursion
        (goto-char (point-min))
        (let ((original-level (string-to-number (org-entry-get (point-min) my-org-promoted-narrowing-lvl-property))))
          (while (> original-level (org-current-level))
            (org-demote-subtree)))
        (org-delete-property my-org-promoted-narrowing-lvl-property)))
  (widen))

(defun my-org-widen-up ()
  (interactive)
  (my-org-demoted-widen)
  (org-up-element)
  (my-org-promoted-narrow))

(defun my-org-map! ()
  (map! :localleader
        (:map org-mode-map
         :n "j n" #'my-org-promoted-narrow
         :n "j w" #'my-org-demoted-widen
         :n "j u" #'my-org-widen-up)))
