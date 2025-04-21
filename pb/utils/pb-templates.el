;;; pb-templates.el --- utils -*- lexical-binding: t; -*-

;; Author: Pierre Baille
;; URL: https://github.com/pbaille
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; pb-templates utility.

;;; Code:

(defvar pb-templates_elisp
  ";;; %1$s.el --- utils -*- lexical-binding: t; -*-

;; Author: Pierre Baille
;; URL: https://github.com/pbaille
;; Version: 0.0.1
;; Package-Requires: ((emacs \"29.1\"))

;;; Commentary:

;; Utils.

;;; Code:

(provide '%1$s)
;;; %1$s.el ends here.
")

(defun pb-templates_elisp-package ()
  "Insert elisp package boilerplate."
  (interactive)
  (point-min)
  (insert (format pb-templates_elisp (file-name-base))))

(provide 'pb-templates)
;;; pb-templates.el ends here.
