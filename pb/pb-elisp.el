;;; pb/pb-elisp.el -*- lexical-binding: t; -*-

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

(provide 'pb-elisp)
