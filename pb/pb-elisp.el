;;; pb-elisp.el --- utils -*- lexical-binding: t; -*-

;; Author: Pierre Baille
;; URL: https://github.com/pbaille
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Utils.

;;; Code:

(defun pb-elisp_send-expression-to-ielm ()
  "Evaluate the expression at point and print the result in the IELM buffer."
  (interactive)
  (if (get-buffer "*ielm*")
      (let* ((expr (or (thing-at-point 'symbol t)
                       (thing-at-point 'sexp t))))
        (with-current-buffer "*ielm*"
          (goto-char (point-max))
          (insert expr)
          (ielm-send-input)))))

(defun pb-elisp_insert-package-prefix ()
  "Insert the package prefix at point based on the current file name."
  (interactive)
  (let* ((file-name (buffer-file-name))
         (base-name (file-name-base file-name))
         ;; Extract the package name (you may need to adjust this based on your file naming convention)
         (package-prefix (concat base-name "_")))
    (insert package-prefix)))

(provide 'pb-elisp)
;;; pb-elisp.el ends here.
