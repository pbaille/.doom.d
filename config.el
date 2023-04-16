;;; config.el -*- lexical-binding: t; -*-

'(load "~/.doom.d/packages/pb/pb-core.el")
'(load "~/.doom.d/packages/pb/pb-misc.el")
'(load "~/.doom.d/packages/pb/pb-s-expr.el")
'(load "~/.doom.d/packages/pb/pb-symex.el")
'(load "~/.doom.d/packages/pb/pb-fennel.el")
'(load "~/.doom.d/packages/pb/pb-reaper.el")

(use-package pb)

(progn :misc

       (add-to-list 'initial-frame-alist '(fullscreen . maximized))

       ;;(setq display-line-numbers-type 'relative)
       (setq display-line-numbers-type t)
       (auto-dim-other-buffers-mode nil)

       ;; hack given by doom creator for a workspace loading issue if i remember well
       (defadvice! enforce-encoding-a (fn &rest args)
         :around #'persp-list-persp-names-in-file
         :around #'persp-load-state-from-file
         :around #'persp-save-state-to-file
         (let ((coding-system-for-read 'utf-8)
               (coding-system-for-write 'utf-8))
           (apply fn args))))

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
         (remove-hook 'clojure-mode-hook #'rainbow-delimiters-mode))
       (progn :colors
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
                      rainbow-ansi-colors nil))))

(progn :lisp

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

       (use-package fennel-mode

         :bind
         (("s-r" . pb/fennel-repl))
         :config
         (setq fennel-mode-switch-to-repl-after-reload nil)
         (set-popup-rules!
           '(("^\\*Fennel REPL" :quit nil :ttl nil))))

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


(use-package company
  :config
  (set-company-backend! 'org-mode nil)
  (setq company-idle-delay 1))

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


(use-package prettier-js
  :config
  (add-hook 'js2-mode-hook 'prettier-js-mode)
  (add-hook 'web-mode-hook 'prettier-js-mode))

















































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
