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

(defun pb-org_toggle-narrow ()
  "Toggle narrowing of current subtree, folding it accordingly."
  (interactive)
  (if (buffer-narrowed-p)
      (org-fold-hide-subtree)
    (org-fold-show-subtree))
  (org-toggle-narrow-to-subtree)
  (recenter))

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
