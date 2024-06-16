;;; pb-fennel.el --- Emacs Lisp integration for Fennel language -*- lexical-binding: t; -*-

;; Author: Pierre Baille
;; URL: https://github.com/pbaille
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1") (fennel-mode "0.9.1"))

;;; Commentary:

;; Fennel helpers.

;;; Code:

(require 'fennel-mode)

;; Lua installation paths
(defvar pb-fennel_lua-5-3-install-path "/usr/local/lib/lua/5.3")
(defvar pb-fennel_lua-5-4-install-path "/usr/local/lib/lua/5.4")

(defun pb-fennel_repl ()
  "Start a new Fennel REPL."
  (interactive)
  (fennel-repl fennel-program))

(defun pb-fennel_reload ()
  "Save the buffer and reload it."
  (interactive)
  (save-buffer)
  (fennel-reload nil))

(defun pb-fennel_eval-buffer ()
  "Evaluate the entire buffer in Fennel."
  (interactive)
  (fennel-eval-region (point-min) (point-max)))

(defun pb-fennel_compile (file)
  "Compile a given FILE in Fennel to Lua."
  (shell-command-to-string (concat fennel-program " -c " file)))

(defun pb-fennel_compile-file (from to)
  "Compile a Fennel file FROM and output it TO a Lua file."
  (shell-command (concat fennel-program " -c " from  " > " to)))

(defun pb-fennel_buffer-file-name-escaped-spaces ()
  "Escape spaces in the buffer file name for shell commands."
  (replace-regexp-in-string " " "\\\\ " (buffer-file-name)))

(defun pb-fennel_compile-fennel (&optional target-dir)
  "Compile the current Fennel buffer to Lua.
When TARGET-DIR is provided, save the compiled Lua file there."
  (interactive)
  (let* ((filename (pb-fennel_buffer-file-name-escaped-spaces))
         (out-file (concat (or target-dir (concat (file-name-parent-directory filename) "/compiled/"))
                           (file-name-base filename)
                           ".lua")))
    (pb-fennel_compile-file filename out-file)))

(defun pb-fennel_install-fennel-script (&optional dir)
  "Compile and save the current Fennel script as a Lua script.
When DIR is provided, save the Lua script there."
  (interactive)
  (let ((s (pb-fennel_compile (pb-fennel_buffer-file-name-escaped-spaces)))
        (lua-filename (concat (file-name-base) ".lua")))
    (pb-misc_spit s (concat (or dir pb-fennel_lua-5-4-install-path) "/" lua-filename))))

(defun pb-fennel_show-fennel-compilation ()
  "Compile and display the current Fennel buffer as Lua."
  (interactive)
  (print (pb-fennel_compile (buffer-file-name-escaped-spaces))))

(provide 'pb-fennel)

;;; pb-fennel.el ends here
