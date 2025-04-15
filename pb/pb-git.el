;;; pb-git.el --- Git utils -*- lexical-binding: t; -*-

;; Author: Pierre Baille
;; URL: https://github.com/pbaille
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1") (magit "3.3.0"))

;;; Commentary:

;; Git helpers.

;;; Code:

(require 'magit)

(defun pb-git_commit-file-change ()
  "Commit current file change."
  (interactive)
  (save-buffer)
  (magit-file-stage)
  (magit-commit-create))

(defun pb-git/stage-file ()
  "Commit current file change."
  (interactive)
  (save-buffer)
  (magit-file-stage))

(defun pb-git/diff-as-string (&optional root-dir)
  "Get the current project's git diff as a string using git command directly."
  (interactive)
  (let ((default-directory (or root-dir (vc-root-dir))))
    (with-temp-buffer
      (call-process "git" nil t nil "diff" "--staged")
      (buffer-string))))

(defun pb-git/file-diff-as-string ()
  "Get the diff for the current file as a string.
   Stages the current file and returns the staged diff output."
  (interactive)
  (if (not buffer-file-name)
      (user-error "Buffer is not visiting a file")
    (let* ((default-directory (vc-root-dir))
           (relative-path (file-relative-name buffer-file-name default-directory)))
      (pb-git/stage-file)
      (with-temp-buffer
        (call-process "git" nil t nil "diff" "--staged" relative-path)
        (buffer-string)))))

(defun pb-git/magit-commit-buffer ()
  (cl-find-if (lambda (buf)
                (and (buffer-file-name buf)
                     (string-match-p "COMMIT_EDITMSG$" (buffer-file-name buf))))
              (buffer-list)))

(defun pb-git/get-diff-string ()
  (let* ((diff-buffers (seq-filter (lambda (buf)
                                     (string-match-p "^magit-diff" (buffer-name buf)))
                                   (buffer-list)))
         (diff-buffer (cond
                       ;; No diff buffers found
                       ((null diff-buffers)
                        (user-error "No magit-diff buffers found. Please open a diff view first"))

                       ;; Exactly one diff buffer - use it automatically
                       ((= 1 (length diff-buffers))
                        (car diff-buffers))

                       ;; Multiple diff buffers - let user choose with completion
                       (t
                        (let* ((buffer-names (mapcar #'buffer-name diff-buffers))
                               (selected-name (completing-read "Choose diff buffer: " buffer-names nil t)))
                          (get-buffer selected-name))))))

    ;; Get content from the selected buffer
    (with-current-buffer diff-buffer
      (buffer-substring-no-properties (point-min) (point-max)))))

(provide 'pb-git)

;;; pb-git.el ends here.
