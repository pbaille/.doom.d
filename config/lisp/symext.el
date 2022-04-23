;;; lisp/symext.el -*- lexical-binding: t; -*-

(require 'symex)

(defun pb/symex-escape-higher ()
  "trying to escape insert mode to symex mode instead of normal evil"
  (interactive)
  (cond ((symex--rigpa-enabled-p)
         (rigpa-enter-higher-level))
        ((symex--evil-enabled-p)
         (evil-normal-state))
        (t (evil-emacs-state))))
