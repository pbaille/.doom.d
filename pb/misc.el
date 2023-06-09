;;; pb/misc.el -*- lexical-binding: t; -*-

(defun pb/open-google ()
  (interactive)
  (xwidget-webkit-browse-url "https://www.google.com/"))

(defun pb/spit (string file)
  "Prints string into file, if files exists, delete it, if not creates it."
  (with-temp-buffer
    (insert string)
    (delete-file file)
    (make-directory (file-name-parent-directory file) t)
    (write-region (point-min) (point-max) file t)))

(defun pb/insert-open-paren ()
  (interactive)
  (insert "()")
  (backward-char)
  '(execute-kbd-macro (kbd "(")))

(defun pb/goto-next-opening-delimiter ()
  (interactive)
  (forward-char)
  (re-search-forward "[(\\[\\{]")
  (backward-char))

(defun pb/goto-prev-opening-delimiter ()
  (interactive)
  (re-search-backward "[(\\[\\{]"))

(defun pb/toggle-level-hiding (arg)
  (interactive "p")
  (hs-life-goes-on
   (if (or (> arg 1) (hs-already-hidden-p))
       (progn (hs-show-block) (hs-hide-level arg))
     (hs-hide-block)))
  (backward-char))

(defun pb/toggle-hiding ()
  (interactive)
  (hs-toggle-hiding)
  (backward-char))

(defun pb/kill-all-dired-buffers ()
  "Kill all dired buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (or (eq major-mode 'dired-mode)
                (eq major-mode 'dired-sidebar-mode))
        (kill-buffer buffer)))))

(defun pb/thing-at-point ()
  (interactive)
  (if (eq (char-syntax (char-after)) ?w)
      (thing-at-point 'symbol)
    (buffer-substring-no-properties
     (point)
     (+ 1 (save-excursion (evil-jump-item) (point))))))

(provide 'pb-misc)
