;;; pb/my-dired.el -*- lexical-binding: t; -*-

(defun my-dired-sidebar-dwim ()
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

(defvar file-renamings-alist nil
  "An alist of all renamings made in Dired. In an attempt to repair broken links.")

(defun pb/dired-create-or-open-dotorg-file ()
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

(defun my-dired-rename-file-advice (file destination ignored)
  "Advice function to save all renamings to `file-renamings-alist`."
  (push (cons file destination) file-renamings-alist))

(advice-add 'dired-rename-file :after #'my-dired-rename-file-advice)

(provide 'my-dired)
