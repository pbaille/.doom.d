;;; pb/utils/pb-meta.el -*- lexical-binding: t; -*-

;;;
;;; pb-meta provides functionality to create and manage metadata files related to
;;; the current buffer or project files. It supports:
;;;
;;; 1. Creating organized directory structures for metadata files
;;; 2. Creating and managing org-mode based documentation
;;; 3. Creating scratch files with proper mode configuration
;;; 4. Finding and navigating between related meta files
;;;
;;; The package uses a "_meta_" directory structure to store related files,
;;; keeping the main working directory clean while maintaining context.
;;;

(require 'f)
(require 'consult)
(require 'org)

(require 'f)
(require 'consult)
(require 'org)

(defgroup pb-meta nil
  "Options for pb-meta."
  :group 'files)

(defcustom pb-meta/directory-name "__"
  "Name of the directory where meta files are stored."
  :type 'string
  :group 'pb-meta)

(progn :utils

       (defun pb-meta/-get-meta-dir (file)
         "Get the meta directory for FILE."
         (let* ((dir (if (f-directory-p file)
                         file
                       (f-dirname file)))
                (meta-dir (f-join dir pb-meta/directory-name)))
           meta-dir))

       (defun pb-meta/-ensure-meta-dir (file)
         "Ensure meta directory exists for FILE."
         (let ((meta-dir (pb-meta/-get-meta-dir file)))
           (unless (f-exists-p meta-dir)
             (f-mkdir meta-dir))
           meta-dir))

       (defun pb-meta/-get-file-basename (file)
         "Get the base name of FILE without extension."
         (file-name-sans-extension (f-filename file)))

       (defun pb-meta/-get-file-meta-dir (file)
         "Get the specific meta directory for FILE.
          If FILE is a directory, return the meta directory without appending a basename."
         (let ((meta-dir (pb-meta/-get-meta-dir file)))
           (if (f-directory-p file)
               meta-dir
             (let ((basename (pb-meta/-get-file-basename file)))
               (f-join meta-dir basename)))))

       (defun pb-meta/-ensure-file-meta-dir (file)
         "Ensure file-specific meta directory exists for FILE."
         (let ((file-meta-dir (pb-meta/-get-file-meta-dir file)))
           (unless (f-exists-p file-meta-dir)
             (f-mkdir-full-path file-meta-dir))
           file-meta-dir))

       (defun pb-meta/-get-current-file ()
         "Get the current file or directory.
          - In dired mode: the file at point or current directory
          - In normal buffers: the buffer's file
          - As fallback: the default directory"
         (cond ((member major-mode '(dired-mode dired-sidebar-mode))
                (dired-get-filename))
               ((buffer-file-name))
               (t default-directory))))

(progn :create

       (defun pb-meta/create-org-file ()
         "Create an org meta file for the current buffer or file at point in dired."
         (interactive)
         (let* ((file (pb-meta/-get-current-file))
                (meta-dir (pb-meta/-ensure-file-meta-dir file))
                (basename (pb-meta/-get-file-basename file))
                (_ (print (kmq meta-dir basename file)))
                (basename (read-string "Meta file name: " basename))
                (org-file (f-join meta-dir (concat basename ".org"))))
           (if (f-exists-p org-file)
               (find-file org-file)
             (find-file org-file)
             (insert (format "* %s\n\n" basename))
             (save-buffer)
             (message "Created new org meta file for %s" basename))))

       (defun pb-meta/create-context-file ()
         "Save current prompt context to a meta file.

          This function creates a 'context.el' file in the meta directory of the
          current buffer's file, saving the current `pb-prompt/context` for later use.

          The saved context can be loaded later with `pb-prompt/load-context-from-file`,
          which is useful for:
          - Preserving project or file-specific prompting context
          - Maintaining consistent LLM interactions across multiple editing sessions
          - Sharing prompt contexts with collaborators

          This integration connects pb-meta's file organization with pb-prompt's
          context management capabilities."
         (interactive)
         (let* ((file (pb-meta/-get-current-file))
                (meta-dir (pb-meta/-ensure-file-meta-dir file))
                (context-file (f-join meta-dir "context.el")))
           (pb-prompt/save-current-context-to-file context-file)
           (message "Saved context to %s" context-file)))

       (defun pb-meta/load-meta-context ()
         "Load a prompt context from the current buffer's meta directory.

          This function loads the context.el file from the meta directory associated
          with the current buffer's file. If the file exists, it will load the context
          using pb-prompt's context loading mechanism and display it in the context browser.

          The function provides a convenient way to:
          - Restore previously saved contexts specific to the current file
          - Continue working with the same LLM interaction context across editing sessions
          - Load shared prompt contexts created by collaborators

          If the context file doesn't exist or the current buffer has no file,
          a message will be displayed."
         (interactive)
         (if (buffer-file-name)
             (let* ((file (buffer-file-name))
                    (meta-dir (pb-meta/-get-file-meta-dir file))
                    (context-file (f-join meta-dir "context.el")))
               (if (f-exists-p context-file)
                   (progn
                     (pb-prompt/load-context-from-file context-file)
                     (pb-prompt/browse-context)
                     (message "Loaded context from %s" context-file))
                 (message "No context file found at %s" context-file)))
           (message "Current buffer has no associated file")))


       (defun pb-meta/create-scratch-file ()
         "Create a scratch file with same extension for the current buffer.
          The scratch file is created in the meta directory associated with
          the current file. Prompts for a custom file name."
         (interactive)
         (let* ((file (buffer-file-name))
                (meta-dir (pb-meta/-ensure-file-meta-dir file))
                (basename (pb-meta/-get-file-basename file))
                (basename (read-string "Scratch file name: " (concat basename "-scratch")))
                (extension (file-name-extension file))
                (scratch-file (f-join meta-dir (concat basename "." extension)))
                (current-mode major-mode)
                (buffer-exists (f-exists-p scratch-file)))
           (find-file scratch-file)
           (unless buffer-exists
             ;; Set appropriate mode based on file extension
             (unless (eq major-mode current-mode)
               (funcall current-mode))
             ;; Insert appropriate header based on file type
             (cond
              ((member extension '("el" "lisp" "cl" "clj" "cljs" "cljc" "edn"))
               (insert (format ";; Scratch file for %s\n\n" (f-filename file))))
              ((member extension '("org"))
               (insert (format "#+TITLE: Scratch for %s\n\n" (f-filename file))))
              (t
               (insert (format "# Scratch file for %s\n\n" (f-filename file)))))
             ;; Special setup for elisp mode
             (when (eq major-mode 'emacs-lisp-mode)
               (insert ";;; -*- lexical-binding: t; -*-\n\n()")
               (goto-char (point-max))
               (when (fboundp 'flycheck-mode)
                 (flycheck-mode -1))
               (when (fboundp 'symex-mode-interface)
                 (symex-mode-interface)))
             (save-buffer)
             (message "Created new scratch file for %s" basename)))))

(progn :find

       (defun pb-meta/change-or-create-meta-file ()
         "Find an existing meta file for the current buffer or create one if none exist.
          Works with the current buffer's file or with file at point in dired."
         (interactive)
         (let* ((file (pb-meta/-get-current-file))
                (meta-dir (pb-meta/-get-file-meta-dir file)))
           (if (and (f-exists-p meta-dir)
                    (not (equal 0 (length (f-files meta-dir nil t)))))
               ;; Meta directory exists and has files - let user select one
               (let* ((meta-files (f-files meta-dir nil t))
                      (file-names (mapcar #'f-filename meta-files))
                      (selected (consult--read
                                 file-names
                                 :prompt "Select meta file: "
                                 :category 'file
                                 :require-match nil
                                 :sort t)))
                 (if (member selected file-names)
                     ;; If selection matches an existing file, open it
                     (find-file (f-join meta-dir selected))
                   ;; If selection doesn't match, create appropriate file
                   (let ((extension (if (string-match "\\.[^.]+$" selected)
                                        (match-string 0 selected)
                                      ".org")))
                     (cond
                      ((string= extension ".org")
                       (let ((org-file (f-join meta-dir selected)))
                         (find-file org-file)
                         (unless (f-exists-p org-file)
                           (insert (format "* %s\n\n" (file-name-sans-extension selected)))
                           (save-buffer)
                           (message "Created new org meta file"))))
                      (t
                       (let ((scratch-file (f-join meta-dir selected))
                             (mode (assoc-default (concat "." (file-name-extension selected t))
                                                  auto-mode-alist #'string-match)))
                         (find-file scratch-file)
                         (when mode (funcall mode))
                         (message "Created new file %s" selected)))))))
             ;; No meta directory or it's empty - prompt to create a file
             (let ((choice (completing-read "Create meta file: "
                                            '("Org Document" "Scratch File" "Context File")
                                            nil t)))
               (cond
                ((string= choice "Org Document")
                 (pb-meta/create-org-file))
                ((string= choice "Scratch File")
                 (pb-meta/create-scratch-file))
                ((string= choice "Context File")
                 (pb-meta/create-context-file)))))))

       (defun pb-meta/goto-main-file ()
         "Navigate back to the main file from a meta file.
          This function attempts to determine the main file associated with
          the current meta file and opens it in a buffer."
         (interactive)
         (let* ((current-file (buffer-file-name))
                (meta-dir-name pb-meta/directory-name)
                (meta-dir-pattern (concat "/" meta-dir-name "/")))
           (if (not (string-match meta-dir-pattern current-file))
               (message "This doesn't appear to be a meta file.")
             ;; Extract the main file name from the path
             (let* ((parts (split-string current-file meta-dir-pattern))
                    (before-meta (car parts))
                    (after-meta (cadr parts))
                    (possible-basename (file-name-sans-extension
                                        (car (split-string after-meta "/"))))
                    (dir-to-search before-meta)
                    (possible-files (directory-files dir-to-search t
                                                     (concat "^" possible-basename "\\."))))
               (if possible-files
                   (find-file (car possible-files))
                 (message "Could not locate the main file for %s" current-file))))))

       (defun pb-meta/find-all-meta-files ()
         "Find any meta file in the project using consult."
         (interactive)
         (let* ((default-directory (or (projectile-project-root) default-directory))
                (meta-dirs (directory-files-recursively default-directory
                                                        (concat "^" pb-meta/directory-name "$")
                                                        t))
                (all-meta-files '()))

           ;; Collect all meta files
           (dolist (dir meta-dirs)
             (dolist (subdir (f-directories dir))
               (let ((files (f-files subdir)))
                 (setq all-meta-files (append all-meta-files files)))))

           (if (null all-meta-files)
               (message "No meta files found in project")
             (let* ((files-with-paths (mapcar (lambda (f)
                                                (cons (concat (file-name-nondirectory
                                                               (directory-file-name (f-dirname f)))
                                                              "/"
                                                              (f-filename f))
                                                      f))
                                              all-meta-files))
                    (selected (consult--read
                               (mapcar #'car files-with-paths)
                               :prompt "Select meta file: "
                               :category 'file
                               :require-match t
                               :sort t))
                    (file-path (cdr (assoc selected files-with-paths))))
               (when file-path
                 (find-file file-path)))))))

(provide 'pb-meta)
