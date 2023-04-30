;;; pb/my-dired.el -*- lexical-binding: t; -*-

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
    (print (list readme-file (file-exists-p readme-file) (directory-file-name focus) (file-name-base (directory-file-name focus))))
    (unless (file-exists-p readme-file)
      (with-temp-file readme-file
        (insert (format "* %s\n\n" (file-name-base (directory-file-name focus)))))) ;; Add secondary header if path is a file

    (with-current-buffer (find-file readme-file)
      (goto-char (point-max))
      '(insert (if (file-directory-p path)
                   ""
                 (format "** %s " (file-name-nondirectory path))))
      (let ((header (if (file-directory-p path)
                        ""
                      (format "** %s " (file-name-nondirectory path)))))

        (when (re-search-backward (format "^%s.*" header) nil t)
          (goto-char (match-beginning 0)))
        (unless (looking-at-p (format "^%s" header))
          (insert header))))))

(defun my-dired-rename-file-advice (file destination ignored)
  "Advice function to save all renamings to `file-renamings-alist`."
  (push (cons file destination) file-renamings-alist))

(advice-add 'dired-rename-file :after #'my-dired-rename-file-advice)

(provide 'my-dired)
