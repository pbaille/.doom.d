;;; elisp-cookbook/echo-repl.el -*- lexical-binding: t; -*-

(defun pb/echo-repl ()
  (interactive)
  (let ((prompt (read-string "prompt: ")))
    (message prompt)
    (if (string-equal prompt "exit")
        (message "exiting")
      (pb/echo-repl))))
