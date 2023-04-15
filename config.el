;;; config.el -*- lexical-binding: t; -*-

(load "~/.doom.d/pb.el")
(progn :misc

       (load "~/.doom.d/parts/re-jump.el")
       (load "~/.doom.d/packages/bencode.el")
       (require 'bencode)

       (add-to-list 'initial-frame-alist '(fullscreen . maximized))

       ;(setq display-line-numbers-type 'relative)
       (setq display-line-numbers-type t)

       (defadvice! enforce-encoding-a (fn &rest args)
         :around #'persp-list-persp-names-in-file
         :around #'persp-load-state-from-file
         :around #'persp-save-state-to-file
         (let ((coding-system-for-read 'utf-8)
               (coding-system-for-write 'utf-8))
           (apply fn args)))

       (use-package webkit-color-picker
         :ensure t
         :bind (("C-c C-p" . webkit-color-picker-show)))

       (use-package kurecolor)

       (use-package rainbow-mode
         :config
         (setq rainbow-html-colors nil
               rainbow-x-colors nil
               rainbow-latex-colors nil
               rainbow-r-colors nil
               rainbow-ansi-colors nil))

       (auto-dim-other-buffers-mode nil)

       (setq-default evil-shift-round nil
                     evil-shift-width 1))

(use-package company
  :config
  (set-company-backend! 'org-mode nil)
  (setq company-idle-delay 1))

(progn :theme
       (after! org
         (progn
           (set-face-attribute 'org-level-1 nil :height 1.4)
           (set-face-attribute 'org-level-2 nil :height 1.2)
           (set-face-attribute 'org-level-3 nil :height 1.1)
           (setq-default line-spacing 3)))
       (load-theme 'doom-horizon-tweaked t)
       ; (load-theme 'doom-ayu-light t)
       (setq doom-font (font-spec :family "Fira Code" :size 14))
       '(highlight-parentheses-mode nil)
       (turn-off-show-smartparens-mode)
       '(global-highlight-parentheses-mode -1)
       (setq rainbow-delimiters-mode nil)
       (after! cc-mode
         (remove-hook 'c-mode-common-hook #'rainbow-delimiters-mode))
       (after! clojure-mode
         (remove-hook 'clojure-mode-hook #'rainbow-delimiters-mode)))

(use-package prettier-js
  :config
  (add-hook 'js2-mode-hook 'prettier-js-mode)
  (add-hook 'web-mode-hook 'prettier-js-mode))

(progn :lisp

       (progn :pb/helpers

              (defun pb/shift-expression (delta)
                (setq evil-shift-width 1)
                (let ((p (point)))
                  (evil-shift-right (if (< delta 0) (- p 1) p)
                                    (save-excursion (evil-jump-item))
                                    delta nil)
                  (goto-char (+ delta p))))

              (defun pb/shift-expression-right ()
                (interactive)
                (pb/shift-expression 1))

              (defun pb/shift-expression-left ()
                (interactive)
                (pb/shift-expression -1))

              (defun pb/indent-sexpr ()
                (interactive)
                (let ((beg (point)))
                  (indent-region beg (save-excursion (evil-jump-item)))
                  (pb/shift-expression (- beg (point)))))

              (defun pb/shift-expressions (delta)
                (let ((p (point)))
                  (while (not (symex--point-at-last-symex-p))
                    (pb/shift-expression delta)
                    (symex-go-forward 1))
                  (pb/shift-expression delta)
                  (goto-char (+ p delta))))

              (defun pb/shift-expressions-right ()
                (interactive)
                (pb/shift-expressions 1))

              (defun pb/shift-expressions-left ()
                (interactive)
                (pb/shift-expressions -1))

              (defun pb/lisp-escape-insert-mode ()
                (interactive)
                (evil-normal-state)
                (forward-char)
                (symex-mode-interface))

              (defun pb/symex-click ()
                (interactive)
                (evil-normal-state)
                (backward-char)
                (symex-mode-interface)

                ;; (evil-normal-state)
                ;; (forward-char)
                ;; (save-excursion (if (hs-already-hidden-p)
                ;;                     (pb/toggle-level-hiding 1)))
                ;; (symex-mode-interface)
                )

              (defun pb/symex-cider-macroexpand ()
                (interactive)
                (save-excursion
                  (evil-normal-state)
                  (evil-jump-item)
                  (forward-char)
                  (cider-macroexpand-1)))

              (defun pb/symex-replace ()
                (interactive)
                (symex-paste-before 1)
                (symex-go-forward 1)
                (symex-delete 1))

              (defun pb/symex-go-up ()
                (interactive)
                (let ((p (point)))
                  (if (hs-already-hidden-p)
                      (pb/toggle-level-hiding 1))
                  (symex-go-up 1)
                  (if (= p (point))
                      (symex-go-forward 1))))

              (defun pb/symex-go-down-folding ()
                (interactive)
                (symex-go-down 1)
                (hs-hide-block)
                (backward-char))

              (defun pb/symex-fw ()
                (interactive)
                (if (hs-already-hidden-p)
                    (progn (print "hidden")
                           (pb/toggle-level-hiding 1)))
                (let ((p (point)))
                  (symex-go-forward 1)
                  (if (and (= p (point))
                           (not (symex--point-at-final-symex-p)))
                      (progn (symex-go-down 1) (pb/symex-fw)))))

              (defun pb/symex-bw ()
                (interactive)
                (let ((p (point)))
                  (symex-go-backward 1)
                  (if (and (= p (point))
                           (not (symex--point-at-initial-symex-p)))
                      (progn (symex-go-down 1) (pb/symex-bw)))))

              (defun pb/symex-next-line (&optional count)
                (interactive)
                (let ((p (point)))
                  (symex-next-visual-line (or count 1))
                  (if (and (<= (point) p) (<= count 10))
                      (pb/symex-next-line (+ 1 (or count 1))))))

              (defun pb/symex-previous-line (&optional count)
                (interactive)
                (let ((p (point)))
                  (symex-previous-visual-line (or count 1))
                  (if (and (>= (point) p) (<= count 10))
                      (pb/symex-previous-line (+ 1 (or count 1))))))

              (defun pb/symex-eval-pp-clojure ()
                (interactive)
                (save-excursion
                  (evil-jump-item)
                  (forward-char)
                  (cider-pprint-eval-last-sexp)
                  (evil-jump-item)))

              (defun pb/symex-tidy-down ()
                (interactive)
                (symex-tidy)
                (symex-go-down 1))

              (defun pb/insert-open-paren ()
                (interactive)
                (insert "()")
                (backward-char)
                '(execute-kbd-macro (kbd "(")))

              (defun pb/goto-next-opening-delimiter ()
                (interactive)
                (forward-char)
                (re-search-forward "[(\\[\\{]")
                (backward-char))

              (defun pb/goto-prev-opening-delimiter ()
                (interactive)
                (re-search-backward "[(\\[\\{]"))

              (defun pb/toggle-level-hiding (arg)
                (interactive "p")
                (hs-life-goes-on
                 (if (or (> arg 1) (hs-already-hidden-p))
                     (progn (hs-show-block) (hs-hide-level arg))
                   (hs-hide-block)))
                (backward-char))

              (defun pb/gradual-unfold ()
                (interactive)
                ())

              (defun pb/toggle-hiding ()
                (interactive)
                (hs-toggle-hiding)
                (backward-char))


              (defun pb/symex-clj-toggle-comment ()
                (interactive)
                ;; this is really badly writen, should find a better way to match #_ under cursor
                (if (looking-at "#_")
                    ;; (and (string-equal (string (following-char)) "#")
                    ;;         (save-excursion (forward-char 1) (string-equal (string (following-char)) "_")))
                    (delete-char 2)
                  (progn (insert "#_")
                         (backward-char 2)))))

       (progn :clojure

              (use-package cider
                :config
                (setq cider-use-overlays t)
                (setq cider-print-fn 'pprint)
                (setq cider-print-options '(("length" 50) ("right-margin" 70)))
                (setq cider-eldoc-display-for-symbol-at-point nil)
                (setq evil-shift-round nil
                      evil-shift-width 1))

              (use-package flycheck-clj-kondo
                :ensure t)

              (use-package clojure-mode
                :ensure t
                :config
                (require 'flycheck-clj-kondo)
                '(copilot-mode nil)))

       (progn :fennel

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
                (let ((s (pb/fennel-compile pb/lua-5-3-install-path))
                      (lua-filename (concat (file-name-base) ".lua")))
                  (pb/spit s (concat pb/lua-5-3-install-path lua-filename))
                  (pb/spit s (concat pb/lua-5-4-install-path lua-filename))))

              (defun pb/show-fennel-compilation ()
                (interactive)
                (print (pb/fennel-compile (buffer-file-name-escaped-spaces))))

              (use-package fennel-mode

                :bind
                (("s-r" . pb/fennel-repl))
                :config
                (setq fennel-mode-switch-to-repl-after-reload nil)
                (set-popup-rules!
                  '(("^\\*Fennel REPL" :quit nil :ttl nil))))

              (progn :reaper

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
                            (setq pb/reaper-osc-client nil)
                            (setq pb/reaper-osc-host "192.168.1.60" )
                            (setq pb/reaper-osc-port 8001)

                            (defun pb/make-reaper-osc-client ()
                              (setq pb/reaper-osc-client
                                    (osc-make-client pb/reaper-osc-host pb/reaper-osc-port)))

                            (defun pb/send-reaper (s)
                              (if (not pb/reaper-osc-client)
                                  (pb/make-reaper-osc-client))
                              (osc-send-message pb/reaper-osc-client s)))

                     (progn :repl

                      (setq pb/reaper-repl-lua-path (concat pb/reaper-user-script-path "repl/DYN.lua"))
                      (setq pb/reaper-repl-fnl-path (concat pb/reaper-user-script-path "repl/DYN.fnl"))
                      (setq pb/reaper-repl-fnl-prelude-path (concat pb/reaper-user-script-path "repl/prelude.fnl"))

                      (setq pb/reaper-repl-tic-message "/repltic") ;; this osc message should be setup as a shortcut on the repl action in reaper
                      (setq pb/reaper-repl-action-id "_RS7d3c_6c93635782a844bfe298cb4e2bffb7256a223058")
                      (setq pb/reaper-repl-start-osc-message (concat "/midiaction/" pb/reaper-repl-action-id))

                      (defun pb/reaper-tic! ()
                        (pb/send-reaper pb/reaper-repl-tic-message))

                      (defun pb/reaper-repl-compile-expression ()
                        (let ((beg (point))
                              (end (save-excursion (evil-jump-item) (+ 1 (point)))))
                          (delete-file pb/reaper-repl-fnl-path)
                          (write-region beg end pb/reaper-repl-fnl-path t)))

                      (defun pb/reaper-repl-sync (in)
                        (interactive)
                        (pb/fennel-compile-file in pb/reaper-repl-lua-path)
                        (sleep-for 0.2)
                        (pb/reaper-tic!))

                      (defun pb/reaper-repl-send-expression ()
                        (interactive)
                        (pb/reaper-repl-compile-expression)
                        (sleep-for 0.2)
                        (pb/reaper-repl-sync pb/reaper-repl-fnl-path))

                      (defun pb/reaper-start-repl ()
                        (interactive)
                        (pb/send-reaper pb/reaper-repl-start-osc-message)
                        (pb/reaper-repl-sync pb/reaper-repl-fnl-prelude-path)))


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
                                    (pb/udp-send-str "127.0.0.1" 9999 "3 + 3")))

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
                                ((fn [a b] (+ a b)) 3 4)))))

       (use-package symex

         :custom
         (symex-modal-backend 'evil)
         (symex-highlight-p nil)

         :bind
         (("s-l" . symex-mode-interface))

         :config
         (setq symex--user-evil-keyspec
               ;; revert k and j in symex
               '(("M-k" . symex-goto-lowest)
                 ("M-j" . symex-goto-highest)
                 ("j" . pb/symex-go-up)
                 ("k" . symex-go-down)
                 ("J" . symex-join-lines)
                 ("K" . pb/symex-go-down-folding)
                 ("M" . pb/symex-cider-macroexpand)
                                        ;("K" . +lookup/documentation)
                 ("s-r" . symex-repl)
                 ("r" . paredit-raise-sexp)
                 ("x" . symex-delete)
                 ("R" . pb/symex-replace)
                 ("T" . pb/toggle-level-hiding) ;; hs-toggle-hiding
                 ("F" . pb/goto-next-opening-delimiter)
                 ("B" . pb/goto-prev-opening-delimiter)
                 ("t" . pb/toggle-hiding)
                 ("E" . pb/symex-eval-pp-clojure)
                 ("C-e" . pb/reaper-repl-send-expression)

                 ("}" . symex-wrap-curly)
                 ("{" . symex-create-curly)

                 ("C-;" . pb/symex-clj-toggle-comment)
                 ("<tab>" . pb/indent-sexpr)
                 ("<backtab>" . symex-tidy)

                 ("h" . symex-go-backward)
                 ("l" . symex-go-forward)
                                        ;("h" . pb/symex-bw)
                                        ;("l" . pb/symex-fw)
                 ("C-k" . pb/symex-previous-line)
                 ("C-j" . pb/symex-next-line)
                                        ;("C-k" . symex-descend-branch)
                                        ;("C-j" . symex-climb-branch)

                                        ;("C" . copilot-hydra/body)
                 ("C->" . pb/shift-expression-right)
                 ("C-<" . pb/shift-expression-left)
                 ("s->" . pb/shift-expressions-right)
                 ("s-<" . pb/shift-expressions-left)

                 ))

         (symex-initialize)

         (setq evil-symex-state-cursor
               '("#B589FE" box))

         (defun symex-eval-clojure ()
           (interactive)
           (cider-eval-last-sexp))

         (general-define-key
          :states 'insert
          :keymaps '(emacs-lisp-mode-map clojure-mode-map fennel-mode-map)
          [escape] #'pb/lisp-escape-insert-mode
          ;; [return] #'symex-mode-interface
          [mouse-1] #'pb/symex-click
                                        ;[double-mouse-1] #'pb/toggle-hiding
          ";" (lambda () (interactive) (print "please use  M-;"))
          "M-;" (lambda () (interactive) (insert ";"))
          "C-w" #'pb/insert-open-paren
          "C-j" #'symex-mode-interface)

         (general-define-key
          :states 'normal
          :keymaps '(emacs-lisp-mode-map clojure-mode-map fennel-mode-map)
          [mouse-1] #'pb/symex-click
                                        ;[double-mouse-1] #'pb/toggle-hiding
          ":" #'re-frame-jump-to-reg
          "C-w" (lambda () (interactive) (evil-insert 1) (pb/insert-open-paren))
          "C-j" #'symex-mode-interface
          "RET" #'symex-mode-interface)

         (general-define-key
          :states 'normal
          :keymaps '(clojure-mode-map)
          ";" #'pb/symex-clj-toggle-comment))


       )

(use-package dired-sidebar
  :ensure t
  :config
  (evil-collection-define-key 'normal 'dired-sidebar-mode-map
    "h" 'dired-sidebar-up-directory
    "l" 'dired-sidebar-find-file))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-d" . dired-jump))
  :config
  (progn
    (add-hook 'dired-mode-hook
              (lambda ()
                (dired-hide-details-mode)
                (dired-sort-toggle-or-edit)))
    (evil-collection-define-key 'normal 'dired-mode-map
      "h" 'dired-up-directory
      "l" 'dired-find-file)))

(load "~/.doom.d/config/org.el")
(load "~/.doom.d/bindings.el")

















































'(comment :scratch-temp

         '(load "~/.doom.d/parts/color-picker/color-picker.el")

         ;; TOODO

         '(use-package! copilot
            ;; :hook (prog-mode . copilot-mode)
            :bind (("C-TAB" . 'copilot-accept-completion-by-word)
                   ("<backtab>" . 'copilot-accept-completion-by-word)
                   :map copilot-completion-map
                   ("<tab>" . 'copilot-accept-completion)
                   ("TAB" . 'copilot-accept-completion)))

         ;; a simple repl that just echo the given prompt until receiving exit
         '(defun pb/echo-repl ()
            (interactive)
            (let ((prompt (read-string "prompt: ")))
              (message prompt)
              (if (string-equal prompt "exit")
                  (message "exiting")
                (pb/echo-repl))))

         ;; setup G binding for gpt/dwim
         '(use-package! gpt
            :config
            (setq gpt-openai-key "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")
            (setq gpt-openai-engine "text-davinci-003")
            :bind (:map evil-normal-state-map ("g p" . gpt-dwim)))

         '(use-package centaur-tabs
            :ensure t
            :config
            (centaur-tabs-mode t)
            (setq centaur-tabs-style "wave")
            (setq centaur-tabs-set-bar 'left)
            (setq centaur-tabs-gray-out-icons 'buffer)
            (setq centaur-tabs-set-icons t)
            (setq centaur-tabs-close-button "x")
            (setq centaur-tabs-set-modified-marker t)
            (setq centaur-tabs-modified-marker "*")
            :bind
            ("C-<left>" . centaur-tabs-backward)
            ("C-<right>" . centaur-tabs-forward)))
