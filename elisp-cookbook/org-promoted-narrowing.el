;;; elisp-cookbook/org-promoted-narrowing.el -*- lexical-binding: t; -*-

(defconst pb-org-promoted-narrowing-lvl-property "ORIGINAL_HEADING_LVL")

(defun pb-org-promoted-narrow ()
  "Narrow to the current subtree and promote it to a top-level heading.
   Store the original level of the heading in a buffer-local variable."
  (interactive)
  (if (buffer-narrowed-p)
      (pb-org-demoted-widen))
  (org-narrow-to-subtree)
  (org-set-property pb-org-promoted-narrowing-lvl-property (number-to-string (org-current-level)))
  (while (> (org-current-level) 1)
    (org-promote-subtree))
  (outline-show-all))

(defun pb-org-demoted-widen ()
  "Widen the buffer and demote the heading to its original level."
  (interactive)
  (if (buffer-narrowed-p)
      (save-excursion
        (goto-char (point-min))
        (let ((original-level (string-to-number (org-entry-get (point-min) pb-org-promoted-narrowing-lvl-property))))
          (while (> original-level (org-current-level))
            (org-demote-subtree)))
        (org-delete-property pb-org-promoted-narrowing-lvl-property)))
  (widen))

(defun pb-org-widen-up ()
  (interactive)
  (pb-org-demoted-widen)
  (org-up-element)
  (pb-org-promoted-narrow))

(defun pb-org-map! ()
  (map! :localleader
        (:map org-mode-map
         :n "j n" #'pb-org-promoted-narrow
         :n "j w" #'pb-org-demoted-widen
         :n "j u" #'pb-org-widen-up)))
