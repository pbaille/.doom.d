;;; pb/my-cider.el -*- lexical-binding: t; -*-

(require 'cider)

(defun my-cider/eval! (code)
  (interactive)
  (cider-interactive-eval code
                          nil nil
                          (cider--nrepl-pr-request-map)))

(provide 'my-cider)
