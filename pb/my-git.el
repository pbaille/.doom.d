;;; pb/my-git.el -*- lexical-binding: t; -*-

(defun my-git-commit-file-change ()
  (interactive)
  (save-buffer)
  (magit-stage-file (buffer-file-name))
  (magit-commit-create))

(provide 'my-git)
