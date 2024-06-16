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
  (magit-stage-file (buffer-file-name))
  (magit-commit-create))

(provide 'pb-git)

;;; pb-git.el ends here.
