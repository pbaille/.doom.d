;;; pb-s-expr.el -*- lexical-binding: t; -*-
;;; pb-sexpr.el --- utils -*- lexical-binding: t; -*-

;; Author: Pierre Baille
;; URL: https://github.com/pbaille
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Utils.

;;; Code:

(require 'symex)

(defun pb-sexpr_shift-expression (delta)
  (setq evil-shift-width 1)
  (let ((p (point)))
    (evil-shift-right (if (< delta 0) (- p 1) p)
                      (save-excursion (evil-jump-item))
                      delta nil)
    (goto-char (+ delta p))))

(defun pb-sexpr_shift-expression-right ()
  (interactive)
  (pb-sexpr_shift-expression 1))

(defun pb-sexpr_shift-expression-left ()
  (interactive)
  (pb-sexpr_shift-expression -1))

(defun pb-sexpr_indent ()
  (interactive)
  (let ((beg (point)))
    (indent-region beg (save-excursion (evil-jump-item)))
    (pb-sexpr_shift-expression (- beg (point)))))

(defun pb-sexpr_shift-expressions (delta)
  (let ((p (point)))
    (while (not (symex--point-at-last-symex-p))
      (pb-sexpr_shift-expression delta)
      (symex-go-forward 1))
    (pb-sexpr_shift-expression delta)
    (goto-char (+ p delta))))

(defun pb-sexpr_shift-expressions-right ()
  (interactive)
  (pb-sexpr_shift-expressions 1))

(defun pb-sexpr_shift-expressions-left ()
  (interactive)
  (pb-sexpr_shift-expressions -1))

(provide 'pb-sexpr)
;;; pb-sexpr.el ends here.
