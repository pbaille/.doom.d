;;; pb/fennel.el -*- lexical-binding: t; -*-

(setq pb/lua-5-3-install-path "/usr/local/lib/lua/5.3/")
(setq pb/lua-5-4-install-path "/usr/local/lib/lua/5.4/")

(defun pb/fennel-repl ()
  (interactive)
  (fennel-repl "fennel"))

(defun pb/fennel-reload ()
  (interactive)
  (save-buffer)
  (fennel-reload nil))

(defun pb/fennel-compile (file)
  (shell-command-to-string (concat "fennel -c " file)))

(defun pb/fennel-compile-file (from to)
  (shell-command (concat "fennel -c " from  " > " to)))

(defun pb/buffer-file-name-escaped-spaces ()
  (replace-regexp-in-string " " "\\\\ " (buffer-file-name)))

(defun pb/compile-fennel (&optional target-dir)
  (interactive)
  (let* ((filename (pb/buffer-file-name-escaped-spaces))
         (out-file (concat (or target-dir (file-name-parent-directory filename))
                           (file-name-base filename))))
    (pb/fennel-compile-file filename out-file)))

(defun pb/install-fennel-script ()
  (interactive)
  (let ((s (pb/fennel-compile (pb/buffer-file-name-escaped-spaces)))
        (lua-filename (concat (file-name-base) ".lua")))
    (pb/spit s (concat pb/lua-5-3-install-path lua-filename))
    ;;(pb/spit s (concat pb/lua-5-4-install-path lua-filename))
    ))

(defun pb/show-fennel-compilation ()
  (interactive)
  (print (pb/fennel-compile (buffer-file-name-escaped-spaces))))

(provide 'my-fennel)
