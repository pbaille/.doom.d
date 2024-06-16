;;; pb-misc.el --- utils -*- lexical-binding: t; -*-

;; Author: Pierre Baille
;; URL: https://github.com/pbaille
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Utils.

;;; Code:

(defun pb-misc_open-google ()
  (interactive)
  (xwidget-webkit-browse-url "https://www.google.com/"))

(defun pb-misc_spit (string file)
  "Prints string into file, if files exists, delete it, if not creates it."
  (with-temp-buffer
    (insert string)
    (delete-file file)
    (make-directory (file-name-parent-directory file) t)
    (write-region (point-min) (point-max) file t)))

(defun pb-misc_insert-open-paren ()
  (interactive)
  (insert "()")
  (backward-char)
  '(execute-kbd-macro (kbd "(")))

(defun pb-misc_goto-next-opening-delimiter ()
  (interactive)
  (forward-char)
  (re-search-forward "[(\\[\\{]")
  (backward-char))

(defun pb-misc_goto-prev-opening-delimiter ()
  (interactive)
  (re-search-backward "[(\\[\\{]"))

(defun pb-misc_toggle-level-hiding (arg)
  (interactive "p")
  (hs-life-goes-on
   (if (or (> arg 1) (hs-already-hidden-p))
       (progn (hs-show-block) (hs-hide-level arg))
     (hs-hide-block)))
  (backward-char))

(defun pb-misc_toggle-hiding ()
  (interactive)
  (hs-toggle-hiding)
  (backward-char))

(defun pb-misc_kill-all-dired-buffers ()
  "Kill all dired buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (or (eq major-mode 'dired-mode)
                (eq major-mode 'dired-sidebar-mode))
        (kill-buffer buffer)))))

(defun pb-misc_count-indentation (line)
  "Count leading spaces in a LINE."
  (if (string-match "^\\s-+" line)
      (length (match-string 0 line))
    0))

(defun pb-misc_remove-leading-spaces (str n)
  "Remove the first N space characters from the beginning of STR."
  (let ((result str)
        (count n))
    (while (and (> count 0) (string-prefix-p " " result))
      (setq result (substring result 1))
      (setq count (1- count)))
    result))

(provide 'pb-misc)
;;; pb-misc.el ends here.