;;; pb-cider.el --- Cider utils -*- lexical-binding: t; -*-

;; Author: Pierre Baille
;; URL: https://github.com/pbaille
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1") (cider "1.15.0"))

;;; Commentary:

;; Cider helpers.

;;; Code:

(require 'cider)

(defun pb-cider_eval! (code)
  "Eval some CODE using cider."
  (interactive)
  (cider-interactive-eval code
                          nil nil
                          (cider--nrepl-pr-request-map)))

(provide 'pb-cider)
;;; pb-cider.el ends here.
