;;; pb/my-fold.el -*- lexical-binding: t; -*-

(defvar my-fold-top-forms '(do progn))

(defun pb/symex-fw-forced ()
  (interactive)
  (let ((p1 (point))
        (p2 (progn (symex-go-forward 1)
                   (point))))
    (if (equal p1 p2)
        (let ((p3 (progn (symex-go-down 1) (point))))
          (if (not (equal p1 p3))
              (pb/symex-fw-forced)))
      :done)))

(defmacro my-fold/top-form-p ()
  "Enclose each element of LST in parentheses."
  `(or ,@(mapcar (lambda (elem) `(looking-at-p ,(format "(%s " elem)))
                 my-fold-top-forms)))

(defun pb/semi-fold-step ()
  (if (my-fold/top-form-p)
      (progn (hs-hide-level 1)
             (symex-go-up 1)
             (pb/symex-fw-forced))
    (pb/symex-fw-forced)))

(defvar my-fold-semi-folded nil)

(defun pb/semi-fold ()
  (interactive)
  (let ((p (point)))
    (hs-hide-all)
    (symex-goto-lowest)
    (symex-goto-first)
    (while (pb/semi-fold-step))
    (setq-local my-fold-semi-folded t)
    (goto-char p)))

(defun pb/toggle-semi-fold ()
  (interactive)
  (if my-fold-semi-folded
      (progn (hs-show-all)
             (setq-local my-fold-semi-folded nil))
    (pb/semi-fold)))

(provide 'my-fold)
