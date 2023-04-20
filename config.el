;;; config.el -*- lexical-binding: t; -*-

(use-package pb)

(progn :misc

       (setq initial-frame-alist
         '((top . 100) (left . 100)
           (width . 200) (height . 65)))

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

(progn :appearance

       (setq-default line-spacing 3)
       (setq doom-font (font-spec :family "Fira Code" :size 14))

       ;; colors copy pasted from horizon theme
       (setq evil-normal-state-cursor '(box "#e95678") ;; red
             evil-insert-state-cursor '(bar "#09f7a0") ;; green
             evil-visual-state-cursor '(hollow "#f09383")) ;; orange

       (load-theme 'doom-horizon-tweaked t)

       (progn :remove-magic-paren
              (turn-off-show-smartparens-mode)
              (remove-hook 'prog-mode-hook
                           #'rainbow-delimiters-mode)
              (after! cc-mode
                (remove-hook 'c-mode-common-hook #'rainbow-delimiters-mode))
              (after! clojure-mode
                (remove-hook 'clojure-mode-hook #'rainbow-delimiters-mode)))

       (progn :colors
              (use-package webkit-color-picker
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

       (defvar pb/lisp-modes
         '(emacs-lisp-mode-map clojure-mode-map fennel-mode-map))

       (progn :clojure

              (use-package cider
                :config
                (setq cider-use-overlays t)
                (setq cider-print-fn 'pprint)
                (setq cider-print-options '(("length" 50) ("right-margin" 70)))
                (setq cider-eldoc-display-for-symbol-at-point nil)
                (setq evil-shift-round nil
                      evil-shift-width 1))

              (use-package flycheck-clj-kondo)

              (use-package re-jump)

              (use-package clojure-mode
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
         (symex-highlight-p t)

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
                 ("M" . pb/symex-cider-macroexpand) ;; +lookup/documentation
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
               '(box "#2bfafa"))

         (defun symex-eval-clojure ()
           (interactive)
           (cider-eval-last-sexp))

         (general-define-key
          :states 'insert
          :keymaps pb/lisp-modes
          [escape] #'pb/symex-escape-insert-mode
          [mouse-1] #'pb/symex-click
          ;;[double-mouse-1] #'pb/toggle-hiding
          ";" (lambda () (interactive) (print "please use  M-;"))
          "M-;" (lambda () (interactive) (insert ";"))
          "C-w" #'pb/insert-open-paren)

         (general-define-key
          :states 'normal
          :keymaps pb/lisp-modes
          [mouse-1] #'pb/symex-click
          ;;[double-mouse-1] #'pb/toggle-hiding
          "C-w" (lambda () (interactive) (evil-insert 1) (pb/insert-open-paren))
          "RET" #'symex-mode-interface)

         (general-define-key
          :states 'normal
          :keymaps '(clojure-mode-map)
          ":" #'re-frame-jump-to-reg
          ";" #'pb/symex-clj-toggle-comment))
       )

(use-package company
  :config
  (set-company-backend! 'org-mode nil)
  (setq company-idle-delay 0.5))

(use-package dired
  :commands (dired dired-jump)
  :bind (("C-d" . dired-jump))
  :config
  (progn
    ;; this fix the 'ls does not supports --dired' error
    (when (string= system-type "darwin")
         (setq dired-use-ls-dired t
               insert-directory-program "/usr/local/bin/gls"
               dired-listing-switches "-aBhl --group-directories-first"))

    (add-hook 'dired-mode-hook
              (lambda ()
                (dired-hide-details-mode)
                (dired-sort-toggle-or-edit)))
    (map! (:map dired-mode-map
                :n "h" 'dired-up-directory
                :n "l" 'dired-find-file
                :n "K" 'dired-subtree-up))))

(use-package dired-sidebar
  :config
  (map! (:map  dired-sidebar-mode-map
         :n "h" 'dired-sidebar-up-directory
         :n "l" 'dired-sidebar-find-file
         :n "q" 'dired-sidebar-hide-sidebar
         :n "Q" 'pb/kill-all-dired-buffers
         :n "K" 'dired-subtree-up)))

(use-package flycheck
  :config
  ;; replace the double arrow of the fringe by a circle
  (define-fringe-bitmap 'flycheck-fringe-bitmap-ball
    (vector #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00111000
            #b01111100
            #b11111110
            #b11111110
            #b01111100
            #b00111000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000))

  (flycheck-define-error-level 'error
    :overlay-category 'flycheck-error-overlay
    :fringe-bitmap 'flycheck-fringe-bitmap-ball
    :fringe-face 'flycheck-fringe-error)

  (flycheck-define-error-level 'warning
    :overlay-category 'flycheck-warning-overlay
    :fringe-bitmap 'flycheck-fringe-bitmap-ball
    :fringe-face 'flycheck-fringe-warning))

(use-package ibuffer
  :ensure t
  :after (doom-themes)
  :config
  ;; tweak appearance
  (add-hook 'ibuffer-mode-hook (lambda () (setq-local line-spacing 6)))
  (setq ibuffer-filter-group-name-face (list :foreground (doom-color 'magenta) :weight 'ultra-bold :height 1.1))
  (setq ibuffer-title-face (list :foreground (doom-blend 'fg 'bg 0.2) :weight 'normal :height 1.1))

  (setq ibuffer-fontification-alist
        `((10 buffer-read-only font-lock-constant-face)
          (15 (and buffer-file-name
                   (string-match ibuffer-compressed-file-name-regexp
                                 buffer-file-name))
              font-lock-doc-face)
          (18 (buffer-modified-p) ((t :foreground ,(doom-color 'yellow) :weight bold)))
          (20 (string-match "^\\*" (buffer-name)) font-lock-keyword-face)
          (25 (and (string-match "^ " (buffer-name))
                   (null buffer-file-name))
              italic)
          (30 (memq major-mode ibuffer-help-buffer-modes) font-lock-comment-face)
          (35 (derived-mode-p 'dired-mode) font-lock-function-name-face)
          (40 (and (boundp 'emacs-lock-mode) emacs-lock-mode) ibuffer-locked-buffer))))

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
