;;; elisp-cookbook/dir-edn.el -*- lexical-binding: t; -*-

(defun my/dired-open-dir-edn-file ()
  "Open or create a 'dir.edn' file in the current directory."
  (interactive)
  (let ((dir-edn-file (concat default-directory "dir.edn")))
    (if (file-exists-p dir-edn-file)
        (find-file dir-edn-file)
      (with-temp-file dir-edn-file
        (insert ""))
      (find-file dir-edn-file)
      (evil-insert 0)
      (yas-expand-snippet "{:description \"${1:Description}\"\n :tags [${2:Tags}]}"))))

(define-key dired-mode-map (kbd "C-c o") 'my/dired-open-dir-edn-file)

(defun my/fd-tagged-files (tags)
  "Search for files in current directory tagged with specified tags using fd."
  (interactive "sEnter tags (space-separated): ")
  (let* ((tag-files-patterns (mapcar (lambda (tag) (concat "*." tag ".tag.edn")) (split-string tags)))
         (tag-files-patterns-regex (mapconcat 'identity tag-files-patterns "\\|"))
         (command (concat "fd -t f -d -x cat '" tag-files-patterns-regex "' | grep -lr '"
                          (dired-current-directory) "' | xargs fd -t f")))
    (dired (split-string (shell-command-to-string command) "\n" t))))

(defun my/fd-tagged-files (tags)
  "Search recursively for files tagged with specified tags using fd."
  (interactive "sEnter tags (space-separated): ")
  (let* ((tags-list (split-string tags))
         (tags-patterns-regex (concat "\\(\\b" (mapconcat 'identity tags-list "\\|") "\\b\\)"))
         (command (concat "fd -t f -x cat 'dir.edn' | grep -rlZ -e '"
                          tags-patterns-regex "' | xargs -0 fd -t f")))
    (dired (split-string (shell-command-to-string command) "\n" t))))

(defun my/fd-tagged-files (tags)
  "Search recursively for files tagged with specified tags using fd."
  (interactive "sEnter tags (space-separated): ")
  (let* ((tags-list (split-string tags))
         (tags-patterns-regex (concat "\\(\\b" (mapconcat 'identity tags-list "\\|") "\\b\\)")))
    (print (concat "fd -t f -d 2 -x cat 'dir.edn' | grep -rlZ -e '"
                   tags-patterns-regex "' | xargs -0 fd -t f"))))
