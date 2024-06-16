;;; pb/pb-git.el -*- lexical-binding: t; -*-

(defun pb-git-commit-file-change ()
  (interactive)
  (save-buffer)
  (magit-stage-file (buffer-file-name))
  (magit-commit-create))

(provide 'pb-git)
