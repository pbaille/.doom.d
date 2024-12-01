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
  "Focus the clicked symex if possible.
If inside string or comment, toggle insert state."
  (interactive)
  (if (lispy--in-string-or-comment-p)
      (evil-insert-state)
    (progn (evil-normal-state)
           (backward-char)
           (symex-mode-interface))))

(defun pb-symex_mark ()
  (interactive)
  (evil-normal-state)
  (activate-mark)
  (push-mark-command nil)
  (deactivate-mark)
  (symex-mode-interface))

(defun pb-symex_undo (&rest _)
  (interactive)
  (evil-undo 1)
  (progn (evil-normal-state)
         (symex-mode-interface)))

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

(defun pb-symex_select-current ()
  "Select the current symex if any."
  (if (cond ((thing-at-point 'string)
             (beginning-of-thing 'string))
            ((thing-at-point 'symbol)
             (beginning-of-thing 'symbol))
            ((thing-at-point 'number)
             (beginning-of-thing 'number))
            ((and (not (lispy-right-p))
                  (thing-at-point 'sexp))
             (beginning-of-thing 'sexp)))
      (and (symex--update-overlay)
           (point))))

(defun pb-symex_select-nearest-bw ()
  "Select the appropriate symex nearest to point."
  (unless (bobp)
    (or
     (pb-symex_select-current)
     (progn (backward-char)
            (pb-symex_select-nearest-bw)))))

(defun pb-symex_select-nearest-fw ()
  "Select the appropriate symex nearest to point."
  (unless (eobp)
    (or (pb-symex_select-current)
        (progn (forward-char)
               (pb-symex_select-nearest-fw)))))

(defun pb-symex_select-nearest ()
  "Select the nearest symex in current line."
  (let ((p (point))
        (fw (save-excursion (pb-symex_select-nearest-fw)))
        (bw (save-excursion (pb-symex_select-nearest-bw))))
    (cond ((and bw fw)
           (if (> (- fw p)
                  (- p bw))
               (goto-char bw)
             (goto-char fw)))
          ((or bw fw)
           (goto-char (or bw fw))))))

(defun pb-symex_select-nearest-in-line-bw ()
  "Select the appropriate symex nearest to point."
  (unless (bolp)
    (or (pb-symex_select-current)
        (progn (backward-char)
               (pb-symex_select-nearest-in-line-bw)))))

(defun pb-symex_select-nearest-in-line-fw ()
  "Select the appropriate symex nearest to point."
  (unless (eolp)
    (or (pb-symex_select-current)
        (progn (forward-char)
               (pb-symex_select-nearest-in-line-fw)))))

(defun pb-symex_select-nearest-in-line ()
  "Select the nearest symex in current line."
  (let ((p (point))
        (fw (save-excursion (pb-symex_select-nearest-in-line-fw)))
        (bw (save-excursion (pb-symex_select-nearest-in-line-bw))))
    (if (cond ((and bw fw)
               (if (> (- fw p)
                      (- p bw))
                   (goto-char bw)
                 (goto-char fw)))

              ((or fw bw)
               (goto-char (or bw fw))))
        (symex--update-overlay))))

(defun pb-symex_next-line (&optional count)
  "Go to next line (or nth if COUNT) and focus the nearest symex."
  (interactive)
  (evil-next-line (or count 1))
  (or (pb-symex_select-nearest-in-line)
      (unless (eobp)
        (pb-symex_next-line 1))))

(defun pb-symex_previous-line (&optional count)
  "Go to previous line (or nth if COUNT) and focus the nearest symex."
  (interactive)
  (evil-previous-line (or count 1))
  (or (pb-symex_select-nearest-in-line)
      (unless (bobp)
        (pb-symex_previous-line 1))))

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

(defun pb-symex_toggle-comment ()
  (interactive)
  ;; this is really badly writen, should find a better way to match #_ under cursor
  (if (or (eq 'clojure-mode major-mode)
          (eq 'clojurec-mode major-mode)
          (eq 'clojurescript-mode major-mode))
      (if (looking-at "#_")
          ;; (and (string-equal (string (following-char)) "#")
          ;;         (save-excursion (forward-char 1) (string-equal (string (following-char)) "_")))
          (delete-char 2)
        (progn (insert "#_")
               (backward-char 2)))
    (symex-comment 1)))

(defun pb-symex_lookup-definition ()
  (interactive)
  (if (or (eq 'clojure-mode major-mode)
          (eq 'clojurec-mode major-mode)
          (eq 'clojurescript-mode major-mode))
      (lsp-find-definition)
    (+lookup/definition)))

(defun pb-symex_lookup-references ()
  (interactive)
  (if (or (eq 'clojure-mode major-mode)
          (eq 'clojurec-mode major-mode)
          (eq 'clojurescript-mode major-mode))
      (lsp-find-references)
    (+lookup/references)))

(provide 'pb-symex)
;;; pb-symex.el ends here.
