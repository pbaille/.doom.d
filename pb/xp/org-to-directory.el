;;; pb/xp/org-to-directory.el --- Export org to directory structure -*- lexical-binding: t; -*-

;; Author: Pierre Baille
;; Keywords: org, markdown, export

(require 'org)
(require 'pb-org)
(require 'ox-md)

;;; Code:

(setq org-export-with-broken-links t
      org-export-with-toc nil
      org-md-headline-style 'atx
      org-export-headline-levels 10)

(defun sanitize-filename (filename)
  "Sanitize FILENAME by replacing invalid characters with underscores."
  (replace-regexp-in-string "[^a-zA-Z0-9-_.]" "-" filename))

(defun pb-org_file-split (file depth)
  "Split an org FILE until DEPTH."
  (with-current-buffer (find-file-noselect file)
    (goto-char (point-min))
    (pb-org_first-node)
    (let* ((base-dir (file-name-as-directory (file-name-sans-extension file))))
      (pb-org_down-element)

      ;; go to the first subheading
      (while (and (not (eq 'headline (pb-org_node-type)))
                  (not (pb-org_last-node-p)))
        (pb-org_forward))

      (when (eq 'headline (pb-org_node-type))
        (make-directory base-dir t)
        (cl-loop for i upfrom 1
                 do
                 (let* ((node-title (org-get-heading t t t t))
                        (file-name (concat base-dir (format "%02d-" i) (sanitize-filename node-title) ".org"))
                        (bounds (pb-org_node-bounds))
                        (content (buffer-substring-no-properties
                                  (car bounds) (cdr bounds))))

                   (progn :write-sub-file
                          (org-narrow-to-subtree)
                          (org-promote-subtree)
                          (let ((content (buffer-substring-no-properties (point-min) (point-max))))
                            (with-temp-file file-name
                              (org-mode)
                              (insert content)))
                          (org-demote-subtree))

                   (progn :erase-content
                          (forward-line)
                          (delete-region (point) (point-max))
                          (goto-char (point-min))
                          (widen))

                   (progn :link-heading
                          (org-edit-headline
                           (format "[[file:%s][%s]]"
                                   (file-relative-name file-name default-directory)
                                   node-title)))

                   (when (> depth 1)
                     (pb-org_file-split file-name (1- depth))))

                 until
                 (not (pb-org_forward)))
        (save-buffer)))))

'(progn
   (pb-org_file-split "~/.doom.d/pb/xp/split-xp.org"
                      2)
   (pb-org_file-split "~/Code/WIP/noon/src/noon/doc/core.org"
                      1))

(progn :clj-doc
       (defun pb-org_convert-to-md-recursively (dir)
         "Recursively export all org files in DIR to markdown."
         (dolist (file (directory-files-recursively dir "\\.org$"))
           (with-current-buffer (find-file-noselect file)
             (org-md-export-to-markdown)
             (delete-file file))))

       (defun pb-org_mk-cljdoc (org-file output-dir depth)
         "Create clj-doc ready folder (OUTPUT-DIR) based on ORG-FILE."
         (if (file-exists-p output-dir)
             (delete-directory output-dir t t))
         (make-directory output-dir t)
         (with-current-buffer (find-file-noselect org-file)
           (org-mode)
           (goto-char (point-min))
           (pb-org_first-node)
           (let* ((title (org-get-heading t t t t))
                  (entry-point (expand-file-name (concat (sanitize-filename title) ".org") output-dir)))
             (copy-file org-file entry-point)
             (pb-org_file-split entry-point depth)))
         '(let ((entry-point (expand-file-name (file-name-nondirectory org-file)
                                               output-dir)))
            (copy-file org-file entry-point)
            (pb-org_file-split entry-point depth))
         (pb-org_convert-to-md-recursively
          output-dir))

       '(progn
          (pb-org_mk-cljdoc
           "~/Code/WIP/noon/src/noon/doc/guide.org"
           "~/Code/WIP/noon/doc"
           2)
          (pb-org_convert-to-md-recursively
           "~/Code/WIP/noon/doc/guide")
          ))

(progn :html-export

       (defun pb-org_copy-and-split (org-file output-dir depth)
         "Create clj-doc ready folder (OUTPUT-DIR) based on ORG-FILE."
         (if (file-exists-p output-dir)
             (delete-directory output-dir t t))
         (make-directory output-dir t)
         (with-current-buffer (find-file-noselect org-file)
           (org-mode)
           (goto-char (point-min))
           (pb-org_first-node)
           (let* ((title (org-get-heading t t t t))
                  (entry-point (expand-file-name (concat (sanitize-filename title) ".org") output-dir)))
             (copy-file org-file entry-point)
             (pb-org_file-split entry-point depth))))

       (defun pb-org_convert-to-html-recursively (dir)
         "Recursively export all org files in DIR to HTML."
         (dolist (file (directory-files-recursively dir "\\.org$"))
           (with-current-buffer (find-file-noselect file)
             (org-html-export-to-html)
             (delete-file file))))

       '(progn :do-it
               (progn
                 (pb-org_copy-and-split
                  "~/Code/WIP/noon/src/noon/doc/guide.org"
                  "~/Code/WIP/noon/public/guide/"
                  2)
                 (pb-org_convert-to-html-recursively
                  "~/Code/WIP/noon/public/guide"))))

'(progn
   (org-md-export-to-markdown ))

(provide 'org-to-directory)


;;; org-to-directory.el ends here
