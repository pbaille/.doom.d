;;; pb-symex.el --- symex utils -*- lexical-binding: t; -*-

;; Author: Pierre Baille
;; URL: https://github.com/pbaille
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1") (symex))

;;; Commentary:

;; symex helpers.

;;; Code:

(require 'symex)

(defun pb-symex_escape-insert-mode ()
  (interactive)
  (evil-normal-state)
  (forward-char)
  (symex-mode-interface))

(defun pb-symex_click ()
  (interactive)
  (evil-normal-state)
  (backward-char)
  (symex-mode-interface)

  ;; (evil-normal-state)
  ;; (forward-char)
  ;; (save-excursion (if (hs-already-hidden-p)
  ;;                     (pb/toggle-level-hiding 1)))
  ;; (symex-mode-interface)
  )

(defun pb-symex_mark ()
  (interactive)
  (evil-normal-state)
  (activate-mark)
  (push-mark-command nil)
  (deactivate-mark)
  (symex-mode-interface))

(defun pb-symex_cider-macroexpand ()
  (interactive)
  (save-excursion
    (evil-normal-state)
    (evil-jump-item)
    (forward-char)
    (cider-macroexpand-1)))

(defun pb-symex_replace ()
  (interactive)
  (symex-paste-before 1)
  (symex-go-forward 1)
  (symex-delete 1))

(defun pb-symex_go-up ()
  (interactive)
  (let ((p (point)))
    (if (hs-already-hidden-p)
        (pb/toggle-level-hiding 1))
    (symex-go-up 1)
    (if (= p (point))
        (symex-go-forward 1))))

(defun pb-symex_go-down-folding ()
  (interactive)
  (symex-go-down 1)
  (hs-hide-block)
  (backward-char))

(defun pb-symex_fw ()
  (interactive)
  (if (hs-already-hidden-p)
      (pb/toggle-level-hiding 1))
  (let ((p (point)))
    (symex-go-forward 1)
    (if (and (= p (point))
             (not (symex--point-at-final-symex-p)))
        (progn (symex-go-down 1) (pb-symex_fw)))))

(defun pb-symex_bw ()
  (interactive)
  (let ((p (point)))
    (symex-go-backward 1)
    (if (and (= p (point))
             (not (symex--point-at-initial-symex-p)))
        (progn (symex-go-down 1) (pb-symex_bw)))))

(defun pb-symex_next-line (&optional count)
  (interactive)
  (let ((p (point)))
    (symex-next-visual-line (or count 1))
    (if (and (<= (point) p) (<= count 10))
        (pb-symex_next-line (+ 1 (or count 1))))))

(defun pb-symex_previous-line (&optional count)
  (interactive)
  (let ((p (point)))
    (symex-previous-visual-line (or count 1))
    (if (and (>= (point) p) (<= count 10))
        (pb-symex_previous-line (+ 1 (or count 1))))))

(defun pb-symex_eval-pp-clojure ()
  (interactive)
  (save-excursion
    (evil-jump-item)
    (forward-char)
    (cider-pprint-eval-last-sexp)
    (evil-jump-item)))

(defun pb-symex_tidy-down ()
  (interactive)
  (symex-tidy)
  (symex-go-down 1))

(defun pb-symex_clj-toggle-comment ()
  (interactive)
  ;; this is really badly writen, should find a better way to match #_ under cursor
  (if (looking-at "#_")
      ;; (and (string-equal (string (following-char)) "#")
      ;;         (save-excursion (forward-char 1) (string-equal (string (following-char)) "_")))
      (delete-char 2)
    (progn (insert "#_")
           (backward-char 2))))

(provide 'pb-symex)
;;; pb-symex.el ends here.
