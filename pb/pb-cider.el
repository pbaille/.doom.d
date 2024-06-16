;;; pb/pb-cider.el -*- lexical-binding: t; -*-

(require 'cider)

(defun pb-cider/eval! (code)
  (interactive)
  (cider-interactive-eval code
                          nil nil
                          (cider--nrepl-pr-request-map)))

(provide 'pb-cider)
