;;; pb-symex.el --- symex utils -*- lexical-binding: t; -*-

;; Author: Pierre Baille
;; URL: https://github.com/pbaille
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1") (symex))

;;; Commentary:

;; symex helpers.

;;; Code:

(require 'symex)


(defun pb-symex/enter ()
  "Enter symex mode and select the nearest expression."
  (interactive)
  (pb-symex/select-nearest-in-line)
  (symex-mode-interface))

(defun pb-symex/escape-insert-mode ()
  "Exit evil insert state and enter symex mode.
   Adjust cursor position for better navigation when exiting near word boundaries."
  (interactive)
  (if (lispy--in-string-p)
      (if (or (looking-at "\"")
              (and (looking-back "\"" 1)
                   (not (looking-back "\\\\" 2))))
          (symex-mode-interface)
        (evil-normal-state))
    (progn
      (forward-char)
      (evil-normal-state)
      (pb-symex/select-nearest-in-line)
      (symex-mode-interface))))

(defun pb-symex/click ()
  "Set cursor to normal state and move back one character."
  (interactive)
  (evil-normal-state)
  (backward-char)
  (when (eq (point)
            (save-excursion (pb-symex/select-nearest-in-line)
                            (point)))
    (symex-mode-interface)))

(defun pb-symex/mark ()
  "Activate mark at current position, then enter symex mode."
  (interactive)
  (evil-normal-state)
  (activate-mark)
  (push-mark-command nil)
  (deactivate-mark)
  (symex-mode-interface))

(defun pb-symex/undo (&rest _)
  "Perform undo operation and return to symex mode."
  (interactive)
  (evil-undo 1)
  (progn (evil-normal-state)
         (symex-mode-interface)))

(defun pb-symex/cider-macroexpand ()
  "Perform CIDER macroexpand-1 on the current symbolic expression."
  (interactive)
  (save-excursion
    (evil-normal-state)
    (evil-jump-item)
    (forward-char)
    (cider-macroexpand-1)))

(defun pb-symex/replace ()
  "Replace current symex with last yanked text."
  (interactive)
  (symex-paste-before 1)
  (symex-go-forward 1)
  (symex-delete 1))

(defun pb-symex/go-up ()
  "Move up one level in symex tree, unfolding if needed."
  (interactive)
  (let ((p (point)))
    (if (hs-already-hidden-p)
        (pb/toggle-level-hiding 1))
    (symex-go-up 1)
    (if (= p (point))
        (symex-go-forward 1))))

(defun pb-symex/go-down ()
  "Move down one level in symex tree, handling org mode specially."
  (interactive)
  (let ((p (point)))
    (symex-go-down 1)
    (if (= (point) p)
        (when (equal major-mode 'org-mode)
          (require 'sorg)
          (pb-org/code-block-goto-beg)
          (evil-sorg-state 1)))))

(defun pb-symex/go-down-folding ()
  "Move down one level in symex tree and fold the block."
  (interactive)
  (symex-go-down 1)
  (hs-hide-block)
  (backward-char))

(defun pb-symex/fw ()
  "Move forward to next symex, unfolding and traversing nested structures."
  (interactive)
  (if (hs-already-hidden-p)
      (pb/toggle-level-hiding 1))
  (let ((p (point)))
    (symex-go-forward 1)
    (if (and (= p (point))
             (not (symex--point-at-final-symex-p)))
        (progn (symex-go-down 1) (pb-symex/fw)))))

(defun pb-symex/bw ()
  "Move backward to previous symex, traversing nested structures."
  (interactive)
  (let ((p (point)))
    (symex-go-backward 1)
    (if (and (= p (point))
             (not (symex--point-at-initial-symex-p)))
        (progn (symex-go-down 1) (pb-symex/bw)))))

(defun pb-symex/select-current ()
  "Select the current symex if any.
   Return point if selection successful, nil otherwise."
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

(progn :nearest

       (defun pb-symex/select-nearest-bw ()
         "Select the appropriate symex nearest to point, searching backward.
          Return point if selection successful."
         (unless (bobp)
           (or
            (pb-symex/select-current)
            (progn (backward-char)
                   (pb-symex/select-nearest-bw)))))

       (defun pb-symex/select-nearest-fw ()
         "Select the appropriate symex nearest to point, searching forward.
          Return point if selection successful."
         (unless (eobp)
           (or (pb-symex/select-current)
               (progn (forward-char)
                      (pb-symex/select-nearest-fw)))))

       (defun pb-symex/select-nearest ()
         "Select the nearest symex to point in any direction.
          Goes to the selected symex's position."
         (let ((p (point))
               (fw (save-excursion (pb-symex/select-nearest-fw)))
               (bw (save-excursion (pb-symex/select-nearest-bw))))
           (cond ((and bw fw)
                  (if (> (- fw p)
                         (- p bw))
                      (goto-char bw)
                    (goto-char fw)))
                 ((or bw fw)
                  (goto-char (or bw fw))))))

       (progn :nearest-in-line

              (defun pb-symex/select-nearest-in-line-bw ()
                "Select the appropriate symex nearest to point in line, searching backward.
                 Return point if selection successful."
                (unless (bolp)
                  (or (pb-symex/select-current)
                      (progn (backward-char)
                             (pb-symex/select-nearest-in-line-bw)))))

              (defun pb-symex/select-nearest-in-line-fw ()
                "Select the appropriate symex nearest to point in line, searching forward.
                 Return point if selection successful."
                (unless (eolp)
                  (or (pb-symex/select-current)
                      (progn (forward-char)
                             (pb-symex/select-nearest-in-line-fw)))))

              (defun pb-symex/multiline-string-p ()
                "Return t if point is inside a multiline string."
                (let ((ppss (syntax-ppss)))
                  (when (nth 3 ppss)    ; Inside a string
                    (let ((string-start (nth 8 ppss))
                          (string-end (or (scan-sexps (nth 8 ppss) 1) (point-max))))
                      (save-excursion
                        (goto-char string-start)
                        (re-search-forward "\n" string-end t))))))

              (defun pb-symex/select-nearest-in-line ()
                "Select the nearest symex in current line.
                 Return t if selection successful."
                (let ((p (point))
                      (fw (save-excursion (pb-symex/select-nearest-in-line-fw)))
                      (bw (save-excursion (pb-symex/select-nearest-in-line-bw))))
                  (if (cond ((and bw fw)
                             (if (> (- fw p)
                                    (- p bw))
                                 (goto-char bw)
                               (goto-char fw)))

                            ((or fw bw)
                             (goto-char (or bw fw))))
                      (symex--update-overlay))))))

(defun pb-symex/next-line (&optional count)
  "Go to next line (or nth if COUNT) and focus the nearest symex."
  (interactive)
  (evil-next-line (or count 1))
  (or (pb-symex/multiline-string-p)
      (pb-symex/select-nearest-in-line)
      (unless (eobp)
        (pb-symex/next-line 1))))

(defun pb-symex/previous-line (&optional count)
  "Go to previous line (or nth if COUNT) and focus the nearest symex."
  (interactive)
  (evil-previous-line (or count 1))
  (or (pb-symex/select-nearest-in-line)
      (unless (bobp)
        (pb-symex/previous-line 1))))

(defun pb-symex/eval-pp-clojure ()
  "Pretty-print evaluate the current Clojure expression."
  (interactive)
  (save-excursion
    (evil-jump-item)
    (forward-char)
    (cider-pprint-eval-last-sexp)
    (evil-jump-item)))

(defun pb-symex/tidy-down ()
  "Tidy the current expression and move down one level."
  (interactive)
  (symex-tidy)
  (symex-go-down 1))

(defun pb-symex/clj-toggle-comment ()
  "Toggle Clojure #_ reader conditional comment on current expression."
  (interactive)
  ;; this is really badly writen, should find a better way to match #_ under cursor
  (if (looking-at "#_")
      ;; (and (string-equal (string (following-char)) "#")
      ;;         (save-excursion (forward-char 1) (string-equal (string (following-char)) "_")))
      (delete-char 2)
    (progn (insert "#_")
           (backward-char 2))))

(defun pb-symex/toggle-comment ()
  "Toggle comment status of current expression based on mode."
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

(defun pb-symex/lookup-definition ()
  "Find definition of symbol at point, using appropriate backend."
  (interactive)
  (when (looking-at "\\W")
    (skip-syntax-forward "^w")
    (when (not (looking-at "\\w"))
      (evil-forward-word-begin)))
  (if (or (eq 'clojure-mode major-mode)
          (eq 'clojurec-mode major-mode)
          (eq 'clojurescript-mode major-mode))
      (if lsp-mode
          (call-interactively #'lsp-find-definition)
        (call-interactively #'+lookup/definition))
    (call-interactively #'+lookup/definition)))

(defun pb-symex/lookup-references ()
  "Find references to symbol at point, using appropriate backend."
  (interactive)
  (when (looking-at "\\W")
    (skip-syntax-forward "^w")
    (when (not (looking-at "\\w"))
      (evil-forward-word-begin)))
  (if (or (eq 'clojure-mode major-mode)
          (eq 'clojurec-mode major-mode)
          (eq 'clojurescript-mode major-mode))
      (if lsp-mode
          (call-interactively #'lsp-find-references)
        (call-interactively #'+lookup/references))
    (call-interactively #'+lookup/references)))

(defun pb-symex/current-as-string (&optional with-properties)
  "Return current symex as string.
   If WITH-PROPERTIES is non-nil, preserve text properties."
  (if with-properties
      (buffer-substring (point) (symex--get-end-point 1))
    (buffer-substring-no-properties (point) (symex--get-end-point 1))))

(defun pb-symex/yank-from-ring ()
  "Replace current symex using consult-yank-from-kill-ring."
  (interactive)
  (evil-insert-state)
  (call-interactively #'consult-yank-from-kill-ring)
  (pb-symex/escape-insert-mode)
  (symex-tidy))

(defun pb-symex/ring-replace ()
  "Replace current symex with text from kill ring via consult interface."
  (interactive)
  (undo-boundary)
  (symex-change 1)
  (pb-symex/yank-from-ring))

(defun pb-symex/ring-append ()
  "Insert text from kill ring after current symex."
  (interactive)
  (symex-append-after)
  (pb-symex/yank-from-ring))

(defun pb-symex/ring-prepend ()
  "Insert text from kill ring before current symex."
  (interactive)
  (symex-insert-before)
  (pb-symex/yank-from-ring))

(defun pb-symex/wrap ()
  "Wrap current symex in parentheses and return to symex mode."
  (interactive)
  (symex-wrap)
  (pb-symex/escape-insert-mode))

(defun pb-symex/raise ()
  "Raise current symex, replacing its parent with it."
  (interactive)
  (paredit-raise-sexp)
  (symex--update-overlay))

(defun pb-symex/delete (&optional count)
  "Delete current symex intelligently.
   With COUNT, delete that many symexes. When deleting the last symex,
   navigate to the previous symex instead of moving up to parent."
  (interactive "p")
  ;; this is taking care of deleting the last symex without poping selection to parent symex
  ;; (going to previous symex instead)
  (let ((count (or count 1)))
    (if (and (symex--point-at-last-symex-p)
             (not (symex--point-at-first-symex-p)))
        (progn (symex-delete count)
               (symex-go-up 1)
               (symex-goto-last))
      (symex-delete count))))

(provide 'pb-symex)
;;; pb-symex.el ends here.
