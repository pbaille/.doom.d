;;; pb/s-expr.el -*- lexical-binding: t; -*-

(defun pb/shift-expression (delta)
  (setq evil-shift-width 1)
  (let ((p (point)))
    (evil-shift-right (if (< delta 0) (- p 1) p)
                      (save-excursion (evil-jump-item))
                      delta nil)
    (goto-char (+ delta p))))

(defun pb/shift-expression-right ()
  (interactive)
  (pb/shift-expression 1))

(defun pb/shift-expression-left ()
  (interactive)
  (pb/shift-expression -1))

(defun pb/indent-sexpr ()
  (interactive)
  (let ((beg (point)))
    (indent-region beg (save-excursion (evil-jump-item)))
    (pb/shift-expression (- beg (point)))))

(defun pb/shift-expressions (delta)
  (let ((p (point)))
    (while (not (symex--point-at-last-symex-p))
      (pb/shift-expression delta)
      (symex-go-forward 1))
    (pb/shift-expression delta)
    (goto-char (+ p delta))))

(defun pb/shift-expressions-right ()
  (interactive)
  (pb/shift-expressions 1))

(defun pb/shift-expressions-left ()
  (interactive)
  (pb/shift-expressions -1))

(defun pb/lisp-escape-insert-mode ()
  (interactive)
  (evil-normal-state)
  (forward-char)
  (symex-mode-interface))
(print "load s-expr")
(provide 'pb-s-expr)
