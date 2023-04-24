;;; pb/my-dired.el -*- lexical-binding: t; -*-

(defun pb/dired-create-or-open-readme-file ()
  "Create or open README.org in directory of current file or directory under cursor in Dired mode."
  (interactive)
  (let ((focus (dired-get-file-for-visit))) ;; get directory of current file
    (if (file-directory-p focus)
        (let ((readme-file (concat focus "/README.org")))
          (unless (file-exists-p readme-file)
            (with-temp-file readme-file
              (insert (format "* %s\n\n" (file-name-base (directory-file-name focus))))))
          (find-file readme-file)))))

(provide 'my-dired)
