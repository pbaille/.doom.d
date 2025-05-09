;;; pb/xp/org-capture-file-infos.el -*- lexical-binding: t; -*-

'(progn (switch-to-buffer "scratch.org")
        (goto-char (org-find-olp (list "~/org/scratch.org" "top" "three" "3.2"))))

(km/defun pb-org/find-or-create-olp-aux
          (:as opts
               file current-path remaining-path)
          (let* ((next-path (append current-path (list (car remaining-path))))
                 (next-marker (ignore-errors (org-find-olp (cons file next-path)))))
            (if next-marker
                (let* ((remaining-next (cdr remaining-path))
                       (next-opts (km/put opts
                                          :marker next-marker
                                          :current-path next-path
                                          :remaining-path remaining-next)))
                  (if remaining-next
                      (pb-org/find-or-create-olp-aux next-opts)
                    next-opts))
              opts)))

(defun pb-org/find-or-create-olp (file path)
  "Find or create outline path PATH in FILE."
  (km/let ((remaining-path marker)
           (pb-org/find-or-create-olp-aux
            (km :file file
                :marker 0
                :current-path ()
                :remaining-path path)))

          (switch-to-buffer (get-file-buffer file))
          (goto-char marker)
          (cl-loop for i in remaining-path
                   do (org-insert-subheading 0) (insert i))))

(defun pb-org/put (file path content)
  "Put CONTENT at PATH in FILE."
  (pb-org/find-or-create-olp file path)
  (cond ((stringp content) (evil-open-below 1) (insert content))
        ((km? content)
         (org-set-tags (km/get content :tags))
         (evil-open-below 1) (insert (km/get content :text)))))

(defvar pb-org/file-infos "~/org/file-infos.org"
  "The main org file to hold file infos.")

(defun pb-org/insert-file-info ()
  "Return the capture target for file info."
  (interactive)
  (pb-org/find-or-create-olp pb-org/file-infos
                             (split-string (buffer-file-name) "/" t))
  (org-narrow-to-subtree)
  (org-end-of-subtree))

'(:org-put-tries
  (pb-org/find-or-create-olp "~/org/scratch.org" (list "top" "tao"))
  (pb-org/find-or-create-olp "~/org/scratch.org" (list "top" "tao" "baz" "iop"))
  (pb-org/put "~/org/scratch.org" (list "top" "tao" "baz" "iop")
              "hello you")
  (pb-org/put "~/org/scratch.org" (list "top" "tao" "baz" "iop")
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
