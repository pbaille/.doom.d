;;; pb.el -*- lexical-binding: t; -*-

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
