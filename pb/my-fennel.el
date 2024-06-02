;;; pb/fennel.el -*- lexical-binding: t; -*-

(require 'fennel-mode)

(setq pb/lua-5-3-install-path "/usr/local/lib/lua/5.3")
(setq pb/lua-5-4-install-path "/usr/local/lib/lua/5.4")

(defun pb/fennel-repl ()
  (interactive)
  (fennel-repl fennel-program))

(defun pb/fennel-reload ()
  (interactive)
  (save-buffer)
  (fennel-reload nil))

(defun pb/fennel-eval-buffer ()
  (interactive)
  (fennel-eval-region (point-min) (point-max)))

(defun pb/fennel-compile (file)
  (shell-command-to-string (concat fennel-program " -c " file)))

(defun pb/fennel-compile-file (from to)
  (shell-command (concat fennel-program " -c " from  " > " to)))

(defun pb/buffer-file-name-escaped-spaces ()
  (replace-regexp-in-string " " "\\\\ " (buffer-file-name)))

(defun pb/compile-fennel (&optional target-dir)
  (interactive)
  (let* ((filename (pb/buffer-file-name-escaped-spaces))
         (out-file (concat (or target-dir (concat (file-name-parent-directory filename) "/compiled/"))
                           (file-name-base filename)
                           ".lua")))
    (pb/fennel-compile-file filename out-file)))

(defun pb/install-fennel-script (&optional dir)
  (interactive)
  (let ((s (pb/fennel-compile (pb/buffer-file-name-escaped-spaces)))
        (lua-filename (concat (file-name-base) ".lua")))
    (pb/spit s (concat (or dir pb/lua-5-4-install-path) "/" lua-filename))))

(defun pb/show-fennel-compilation ()
  (interactive)
  (print (pb/fennel-compile (buffer-file-name-escaped-spaces))))

(provide 'my-fennel)
