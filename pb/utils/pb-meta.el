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
(require 'pb-misc)

(defgroup pb-meta nil
  "Options for pb-meta."
  :group 'files)

(defcustom pb-meta/directory-name "__"
  "Name of the directory where meta files are stored."
  :type 'string
  :group 'pb-meta)

(progn :utils

       (defun pb-meta/get-meta-dir (file)
         "Get the meta directory for FILE."
         (f-join (f-dirname file)
                 pb-meta/directory-name
                 (f-filename file)))

       (defun pb-meta/ensure-meta-dir (file)
         "Ensure meta directory exists for FILE."
         (let ((meta-dir (pb-meta/get-meta-dir file)))
           (unless (f-exists-p meta-dir)
             (condition-case _ (f-mkdir-full-path meta-dir) (error nil)))
           meta-dir))

       (defun pb-meta/meta-file? (file)
         "Check if FILE is a meta file.
          Return non-nil if FILE is located in a meta directory."
         (when file
           (let ((meta-dir-pattern (concat "/" pb-meta/directory-name "/")))
             (string-match-p meta-dir-pattern file))))

       (require 'pb-misc)

       (defun pb-meta/get-main-file (&optional file)
         (interactive)
         (let* ((current-file (or file (pb-misc/get-current-file)))
                (meta-dir-name pb-meta/directory-name)
                (meta-dir-pattern (concat "/" meta-dir-name "/")))
           (if (string-match meta-dir-pattern current-file)
               (let* ((parts (split-string current-file meta-dir-pattern))
                      (before-meta (car parts))
                      (after-meta (cadr parts)))
                 (f-join before-meta (car (split-string after-meta "/"))))
             current-file)))

       (defun pb-meta/get-parent-meta-dirs (file)
         "Get the list of all existing parent meta directories of FILE."
         (let* ((parents (f-split (pb-meta/get-main-file file)))
                (current-path "")
                (meta-dirs '()))
           ;; Build paths from root to the file location
           (dolist (part parents)
             (setq current-path (f-join current-path part))
             (let ((meta-path (f-join current-path pb-meta/directory-name)))
               ;; If a meta directory exists at this level, add it to the list
               (when (f-directory-p meta-path)
                 (push meta-path meta-dirs))))
           ;; Return directories in order from closest to furthest
           (nreverse meta-dirs))))

(progn :create

       (defun pb-meta/create-org-file (&optional custom-filename)
         "Create an org meta file for the current buffer or file at point in dired.
          If CUSTOM-FILENAME is provided, use that instead of prompting."
         (interactive)
         (let* ((file (pb-misc/get-current-file))
                (meta-dir (pb-meta/ensure-meta-dir file))
                (basename (f-filename file))
                (basename (if custom-filename
                              (file-name-sans-extension custom-filename)
                            (read-string "Meta file name: " basename)))
                (org-file (f-join meta-dir (concat basename ".org"))))
           (if (f-exists-p org-file)
               (find-file org-file)
             (find-file org-file)
             (insert (format "* %s\n\n" basename))
             (save-buffer)
             (message "Created new org meta file for %s" basename))))

       (defun pb-meta/create-scratch-file (&optional custom-filename)
         "Create a scratch file with same extension for the current buffer.
          The scratch file is created in the meta directory associated with
          the current file. If CUSTOM-FILENAME is provided, use that instead of prompting."
         (interactive)
         (let* ((file (pb-misc/get-current-file))
                (meta-dir (pb-meta/ensure-meta-dir file))
                (basename (f-filename file))
                (basename (if custom-filename
                              (file-name-sans-extension custom-filename)
                            (read-string "Scratch file name: " (concat basename "-scratch"))))
                (extension (if custom-filename
                               (file-name-extension custom-filename)
                             (file-name-extension file)))
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
             (message "Created new scratch file for %s" basename))))

       (defun pb-meta/change-or-create-meta-file ()
         "Find an existing meta file for the current buffer or create one if none exist.
          Works with the current buffer's file or with file at point in dired."
         (interactive)
         (let* ((file (pb-misc/get-current-file))
                (meta-dir (pb-meta/ensure-meta-dir file))
                (meta-files (f-files meta-dir nil t))
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
             (cond
              ;; Org files
              ((string-match "\\.org$" selected)
               (pb-meta/create-org-file selected))
              ;; All other files treated as scratch files
              (t
               (pb-meta/create-scratch-file selected)))))))

(progn :find

       (defun pb-meta/goto-main-file ()
         "Navigate back to the main file from a meta file.
          This function attempts to determine the main file associated with
          the current meta file and opens it in a buffer."
         (interactive)
         (if-let* ((main-file (pb-meta/get-main-file)))
             (find-file main-file)
           (message "Could not locate the main file for %s" current-file)))

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
