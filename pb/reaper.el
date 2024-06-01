;;; pb/reaper.el -*- lexical-binding: t; -*-

(require 'bencode)

(setq pb/reaper-user-script-path "/Users/pierrebaille/Library/Application\ Support/REAPER/Scripts/PB/")
(setq pb/lua-script-path "/Users/pierrebaille/Code/Lua/")

(define-minor-mode reaper-mode
  "Get your foos in the right places."
  :lighter " reaper"
  :keymap (make-sparse-keymap))

(defun pb/reaper-mode ()
  (interactive)
  (reaper-mode t))

(defun pb/replace-filename-extension (filename ext)
  (concat (file-name-sans-extension filename) "." ext))

(defun pb/compile-reascript ()
  (interactive)
  (let ((p (buffer-file-name)))
    (assert (string-prefix-p pb/lua-script-path p)
            "only compile from lua-script-path")
    (let ((s (pb/fennel-compile p))
          (subpath (string-remove-prefix pb/lua-script-path p)))
      (pb/spit s (concat pb/lua-script-path
                         "compiled/"
                         (pb/replace-filename-extension subpath "lua"))))))

(progn :osc

       (defun pb/get-local-ip ()
           (string-trim-right (shell-command-to-string "ipconfig getifaddr en0")))

       (setq pb/reaper-osc-client nil)
       (setq pb/reaper-osc-host (pb/get-local-ip))
       (setq pb/reaper-osc-port 8001)

       (defun pb/make-reaper-osc-client ()
         (setq pb/reaper-osc-client
               (osc-make-client pb/reaper-osc-host pb/reaper-osc-port)))

       (defun pb/send-reaper (s)
         (if (not pb/reaper-osc-client)
             (pb/make-reaper-osc-client))
         (osc-send-message pb/reaper-osc-client s)))

(progn :socket

       (defun pb/mk-udp-socket (ip port)
         (make-network-process :name "udp-socket" :type 'datagram :family 'ipv4 :host ip :service port))

       (defun pb/udp-socket-send-str (socket str)
         (process-send-string socket (encode-coding-string str 'utf-8)))

       (defun pb/udp-send-str (ip port str)
         (let ((sk (pb/mk-udp-socket ip port)))
           (pb/udp-socket-send-str sk str)
           (delete-process sk)))

       '(progn "reaper-ping"
               (pb/udp-send-str "127.0.0.1" 9999 "3 + 3")
               (pb/udp-send-str "127.0.0.1" 9999 (bencode-encode '(:code "(+ 1 2)")))))

(progn :socket-repl

        (setq pb/reaper-socket-repl-host "127.0.0.1")
        (setq pb/reaper-socket-repl-port 9999)

        (defun pb/current-s-expression-as-string ()
          (interactive)
          (buffer-substring-no-properties
           (point)
           (+ 1 (save-excursion (evil-jump-item) (point)))))

        (defun pb/send-to-reaper-socket-repl (s)
          (pb/udp-send-str pb/reaper-socket-repl-host
                           pb/reaper-socket-repl-port
                           s))

        (defun pb/send-fnl-s-expression-to-reaper-socket-repl ()
          (interactive)
          (let ((s (pb/current-s-expression-as-string))
                (f (make-temp-file "fnl-code-" nil ".fnl") ))
            (with-temp-file f (insert s))
            (pb/send-to-reaper-socket-repl
             (bencode-encode `(:code ,s
                               :compiled ,(pb/fennel-compile f))))
            (delete-file f)))

        '(progn :try
                ((fn [a b] (+ a b)) 3 4)))

(provide 'pb-reaper)
