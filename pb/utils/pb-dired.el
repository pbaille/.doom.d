;;; pb-dired.el --- utils -*- lexical-binding: t; -*-

;; Author: Pierre Baille
;; URL: https://github.com/pbaille
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Utils.

;;; Code:

(require 'evil)
(require 'dired)
(require 'dired-subtree)

(defun pb-dired/sidebar-dwim ()
  "Visit the buffer on this line.
If optional argument SINGLE is non-nil, then also ensure there is only
one window."
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (when file
      (if (file-directory-p file)
          (dired-subtree-toggle)
        (let ((buffer (or (find-buffer-visiting file)
                          (find-file-noselect file))))
          (when buffer
            (windmove-right)
            (switch-to-buffer buffer)))))))

(defun pb-dired/sidebar-mouse-dwim (event)
  "Perform an action on mouse click in dired-sidebar."
  (interactive "e")
  ;; Perform your desired action here, such as opening the file or executing a command.
  ;; For example, you can use `(dired-sidebar-goto-file-other-window)` to open the file in another window.

  ;; Preserve the cursor position
  (mouse-set-point event)
  (print "pb-dired/mouse")
  (pb-dired/sidebar-dwim)
  )

(defun pb-dired/sidebar-close-all ()
  "Close all dired-sidebar buffers before exiting Emacs."
  (interactive)
  (walk-windows
   (lambda (win)
     (when (and (window-dedicated-p win)
                (string-match-p "^:~/" (buffer-name (window-buffer win))))
       (delete-window win))))
  (dolist (buf (buffer-list))
    (when (string-match-p "^:~/" (buffer-name buf))
      (kill-buffer buf))))

(advice-add #'doom/quicksave-session :before #'pb-dired/sidebar-close-all)
(add-hook 'kill-emacs-hook #'pb-dired/sidebar-close-all)

(defun pb-dired/sidebar-reset ()
  (interactive)
  (pb-misc/kill-all-dired-buffers)
  (dired-sidebar-toggle-sidebar))

(advice-add #'dired-sidebar-mouse-subtree-cycle-or-find-file :override #'pb-dired/sidebar-mouse-dwim)

(defvar pb-dired/file-renamings-alist nil
  "An alist of all renamings made in Dired. In an attempt to repair broken links.")

(defun pb-dired/create-or-open-dotorg-file ()
  "Create or open .org in directory of current file or directory under cursor in Dired mode."
  (interactive)
  (let* ((path (dired-get-file-for-visit))
         (focus (if (file-directory-p path)
                    path
                  (file-name-directory path))) ;; get directory of current file
         (readme-file (expand-file-name ".org" focus)))

    (unless (file-exists-p readme-file)
      (with-temp-file readme-file
        (insert (format "* %s\n\n" (file-name-base (directory-file-name focus))))))

    ;; Add secondary header if path is a file

    (with-current-buffer (find-file readme-file)
      (goto-char (point-max))
      (let ((p (point))
            (beg-of-line (save-excursion (beginning-of-line) (point))))
        (if (not (equal p beg-of-line))
            (evil-insert-newline-above)))
      (let ((header (if (file-directory-p path)
                        ""
                      (format "** [[%s][%s]]"
                              path
                              (file-name-nondirectory path)))))

        (when (re-search-backward (format "^%s.*" header) nil t)
          (goto-char (match-beginning 0)))
        (unless (looking-at-p (format "^%s" header))
          (insert header))))))

(defun pb-dired/rename-file-advice (file destination ignored)
  "Advice function to save all renamings to `pb-dired/file-renamings-alist`."
  (push (cons file destination) pb-dired/file-renamings-alist))

(advice-add 'dired-rename-file :after #'pb-dired/rename-file-advice)

(provide 'pb-dired)
;;; pb-dired.el ends here.
