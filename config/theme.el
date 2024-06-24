;;; config/theme.el -*- lexical-binding: t; -*-

(setq-default line-spacing 3)
(setq doom-font
      (font-spec :family "Fira Code" :size 14))

;; colors copy pasted from horizon theme
(setq evil-normal-state-cursor
      '(box "#e95678") ;; red
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
         (remove-hook 'clojure-mode-hook #'rainbow-delimiters-mode))
       (remove-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode))

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
               rainbow-ansi-colors nil)))
