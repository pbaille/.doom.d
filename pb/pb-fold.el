;;; pb-fold.el --- Fold utils -*- lexical-binding: t; -*-

;; Author: Pierre Baille
;; URL: https://github.com/pbaille
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Fold utils.

;;; Code:


(defvar pb-fold_top-forms '(do progn))

(defun pb-fold_symex-fw-forced ()
  "Force forward move."
  (let ((p1 (point))
        (p2 (progn (symex-go-forward 1)
                   (point))))
    (if (equal p1 p2)
        (let ((p3 (progn (symex-go-down 1) (point))))
          (if (not (equal p1 p3))
              (pb-fold_symex-fw-forced)))
      :done)))

(defmacro pb-fold_top-form-p ()
  "Enclose each element of LST in parentheses."
  `(or ,@(mapcar (lambda (elem) `(looking-at-p ,(format "(%s " elem)))
                 pb-fold_top-forms)))

(defun pb-fold_semi-fold-step ()
  "Semi fold step."
  (if (pb-fold_top-form-p)
      (progn (hs-hide-level 1)
             (symex-go-up 1)
             (pb-fold_symex-fw-forced))
    (pb-fold_symex-fw-forced)))

(defvar pb-fold_semi-folded nil)

(defun pb-fold_semi-fold ()
  "Semi fold."
  (interactive)
  (let ((p (point)))
    (hs-hide-all)
    (symex-goto-lowest)
    (symex-goto-first)
    (while (pb-fold_semi-fold-step))
    (setq-local pb-fold_semi-folded t)
    (goto-char p)))

(defun pb-fold_toggle-semi-fold ()
  "Toggle semi fold."
  (interactive)
  (if pb-fold_semi-folded
      (progn (hs-show-all)
             (setq-local pb-fold_semi-folded nil))
    (pb-fold_semi-fold)))

(provide 'pb-fold)

;;; pb-fold.el ends here.
