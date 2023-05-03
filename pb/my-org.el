;;; pb/my-org.el -*- lexical-binding: t; -*-

;; Utility belt for manipulating org files
;; The initial motivation is to build a system for adding metadata to files and dirs

;; olp stands for outline path
;; this switches to the scratch org buffer and go to one of its sub header

(require 'km)
(require 'org)

'(progn (switch-to-buffer "scratch.org")
        (goto-char (org-find-olp (list "~/org/scratch.org" "top" "three" "3.2"))))

(defnk my-org-find-or-create-olp-aux
  (:as opts
       file current-path remaining-path)
  (let* ((next-path (append current-path (list (car remaining-path))))
         (next-marker (ignore-errors (org-find-olp (cons file next-path)))))
    (if next-marker
        (let* ((remaining-next (cdr remaining-path))
               (next-opts (km-put opts
                                  :marker next-marker
                                  :current-path next-path
                                  :remaining-path remaining-next)))
          (if remaining-next
              (my-org-find-or-create-olp-aux next-opts)
            next-opts))
      opts)))

(defun my-org-find-or-create-olp (file path)
  (km-letk ((remaining-path marker)
            (my-org-find-or-create-olp-aux
             (km :file file
                 :marker 0
                 :current-path ()
                 :remaining-path path)))

           (switch-to-buffer (get-file-buffer file))
           (goto-char marker)
           (cl-loop for i in remaining-path
                    do (org-insert-subheading 0) (insert i))))

(defun my-org-put (file path content)
  (my-org-find-or-create-olp file path)
  (cond ((stringp content) (evil-open-below 1) (insert content))
        ((km? content)
         (org-set-tags (km-get content :tags))
         (evil-open-below 1) (insert (km-get content :text)))))

(defvar my-org-file-infos "~/org/file-infos.org"
  "the main org file to hold file infos")

(defun my-org-insert-file-info ()
  "Return the capture target for file info."
  (interactive)
  (my-org-find-or-create-olp my-org-file-infos
                             (split-string (buffer-file-name) "/" t))
  (org-narrow-to-subtree)
  (org-end-of-subtree))

'(:org-put-tries
  (my-org-find-or-create-olp "~/org/scratch.org" (list "top" "tao"))
  (my-org-find-or-create-olp "~/org/scratch.org" (list "top" "tao" "baz" "iop"))
  (my-org-put "~/org/scratch.org" (list "top" "tao" "baz" "iop")
              "hello you")
  (my-org-put "~/org/scratch.org" (list "top" "tao" "baz" "iop")
              (km :tags (list "pouet" "foo")
                  :text "very interesting indeed")))

'(:capture-hook-xp
  (defun my-after-capture-action ()
    (print (file-name-parent-directory (org-capture-get :original-file)))
    (org-refile nil nil (list "a" "file-infos.org" nil nil)))

  (add-hook 'org-capture-before-finalize-hook
            #'my-after-capture-action)
  (remove-hook 'org-capture-before-finalize-hook
               #'my-after-capture-action))

(provide 'my-org)
