;;; pb/utils/pb-consult.el -*- lexical-binding: t; -*-

(require 'consult)

(progn :consult-project-file

       (setq pb-consult/source-project-file
             `( :name     "Project File"
                :narrow   ?f
                :category file
                :face     consult-file
                :history  file-name-history
                :state    ,#'consult--file-state
                :new
                ,(lambda (file)
                   (consult--file-action
                    (expand-file-name file (consult--project-root))))
                :items
                ,(lambda ()
                   (when-let (root (consult--project-root))
                     ;; Return cons cells: (relative-path . absolute-path)
                     ;; Sort by path length so shorter paths appear first
                     (let ((files (projectile-project-files root)))
                       (setq files (sort files (lambda (a b) (< (length a) (length b)))))
                       (mapcar (lambda (file)
                                 (cons file (expand-file-name file root)))
                               files))))
                ;; Use cdr to get the absolute path from cons cell
                :lookup
                ,(lambda (selected candidates input narrow)
                   (if (consp selected) (cdr selected) selected))
                ;; Sort by path length - shorter paths appear first
                :sort t))

       (defun pb-consult/project-file ()
         (interactive)
         (consult--with-project
           (consult-buffer '(pb-consult/source-project-file)))))


(progn :consult-project-recent-file

       (setq pb-consult/source-project-recent-file
             `( :name     "Recent Project File"
                :narrow   ?r
                :category file
                :face     consult-file
                :history  file-name-history
                :state    ,#'consult--file-state
                :new
                ,(lambda (file)
                   (consult--file-action
                    (expand-file-name file (consult--project-root))))
                :enabled
                ,(lambda ()
                   (and consult-project-function
                        recentf-mode))
                :items
                ,(lambda ()
                   (when-let (root (consult--project-root))
                     (let ((len (length root))
                           (ht (consult--buffer-file-hash))
                           items)
                       (dolist (file (bound-and-true-p recentf-list))
                         ;; Emacs 29 abbreviates file paths by default, see
                         ;; `recentf-filename-handlers'.  I recommend to set
                         ;; `recentf-filename-handlers' to nil to avoid any slow down.
                         (unless (eq (aref file 0) ?/)
                           (let (file-name-handler-alist) ;; No Tramp slowdown please.
                             (setq file (expand-file-name file))))
                         (when (string-prefix-p root file)
                           (let ((part (substring file len)))
                             (when (equal part "") (setq part "./"))
                             (push (cons part file) items))))
                       ;; Sort by path length so shorter paths appear first
                       (sort items (lambda (a b) (< (length (car a)) (length (car b))))))))
                ;; Use cdr to get the absolute path from cons cell
                :lookup
                ,(lambda (selected candidates input narrow)
                   (if (consp selected) (cdr selected) selected))
                ;; Sort by path length - shorter paths appear first
                :sort t))

       (defun pb-consult/recent-project-file ()
         "Find and open a recent project file using consult.
          
          Uses recentf to show only recently opened files from the current project,
          with full path matching and preview enabled."
         (interactive)
         (consult--with-project
           (consult-buffer '(pb-consult/source-project-recent-file))))

       )


(progn :consult-project-buffer

       (setq pb-consult/source-project-buffer
             `( :name     "Project Buffer"
                :narrow   ?b
                :category buffer
                :face     consult-buffer
                :history  buffer-name-history
                :state    ,#'consult--buffer-state
                :default  t
                :items
                ,(lambda ()
                   (when-let (root (consult--project-root))
                     ;; Use consult's own buffer-query logic, filter out dired buffers, then format
                     (mapcar (lambda (buffer)
                               (let ((buffer-name (buffer-name buffer))
                                     (file-name (buffer-file-name buffer)))
                                 (if file-name
                                     (let* ((relative-path (file-relative-name file-name root))
                                            (relative-dir (file-name-directory relative-path)))
                                       ;; Only add prefix if file is actually in the project
                                       (if (or (string-prefix-p ".." relative-path)
                                               (not relative-dir))
                                           (cons buffer-name buffer)
                                         (cons (format "%s %s" 
                                                       buffer-name 
                                                       (propertize (directory-file-name relative-dir) 
                                                                   'face (list :foreground (doom-darken (doom-color 'fg) 0.5))))
                                               buffer)))
                                   ;; Non-file buffers just use their name
                                   (cons buffer-name buffer))))
                             (seq-filter (lambda (buffer)
                                           (let ((mode (buffer-local-value 'major-mode buffer)))
                                             (not (memq mode '(dired-mode dired-sidebar-mode)))))
                                         (consult--buffer-query :sort 'visibility
                                                                :directory root)))))
                ;; Use cdr to get the actual buffer from cons cell
                :lookup
                ,(lambda (selected candidates input narrow)
                   (if (consp selected) (cdr selected) selected))))

       (defun pb-consult/project-buffer ()
         "Buffer switcher for project buffers with relative path prefixes.
          
          Shows buffers from the current project with their relative paths
          displayed alongside the buffer name."
         (interactive)
         (consult--with-project
           (consult-buffer '(pb-consult/source-project-buffer)))))

(provide 'pb-consult)
