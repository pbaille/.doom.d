;;; config/theme.el -*- lexical-binding: t; -*-

(setq-default line-spacing 3)

(setq doom-font
      (font-spec :family "Fira Code" :size 14))

;; colors copy pasted from horizon theme
(setq evil-normal-state-cursor
      '(box "#e95678") ;; red
      evil-insert-state-cursor '(bar "#09f7a0") ;; green
      evil-visual-state-cursor '(hollow "#f09383")) ;; orange

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-horizon-tweaked t)
  (setq doom-theme 'doom-horizon-tweaked))

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

       (use-package webkit-color-picker)

       (use-package kurecolor)

       (use-package rainbow-mode
         :config
         (setq rainbow-html-colors nil
               rainbow-x-colors nil
               rainbow-latex-colors nil
               rainbow-r-colors nil
               rainbow-ansi-colors nil)))

(require 'spacious-padding)
;; redefine brutally
(defun spacious-padding-set-faces (&rest _)
  "Make window dividers invisible and add padding.
Ignore any arguments.  This is useful to add the function to abnormal
hooks that pass one or more arguments to it, such as
`after-make-frame-functions'."
  (let ((bg-main "#1a1c23")
        (fg-main (face-foreground 'default))
        custom--inhibit-theme-enable)
    (custom-theme-set-faces
     'spacious-padding
     `(fringe ((t :background ,(face-background 'default))))
     `(line-number ((t :background ,bg-main)))
     `(header-line ((t ,@(spacious-padding-set-face-box-padding 'header-line 'default))))
     `(header-line-highlight ((t :box (:color ,fg-main))))
     `(keycast-key ((t ,@(spacious-padding-set-face-box-padding 'keycast-key 'default))))
     `(mode-line ((t ,@(spacious-padding-set-face-box-padding 'mode-line 'default :mode-line-active))))
     ;; We cannot use :inherit mode-line because it does not get our version of it...
     `(mode-line-active ((t ,@(spacious-padding-set-face-box-padding 'mode-line-active 'mode-line :mode-line-active))))
     `(mode-line-inactive ((t ,@(spacious-padding-set-face-box-padding 'mode-line-inactive 'mode-line :mode-line-inactive))))
     `(mode-line-highlight ((t :box (:color ,fg-main))))
     `(tab-bar-tab ((t ,@(spacious-padding-set-face-box-padding 'tab-bar-tab 'tab-bar))))
     `(tab-bar-tab-inactive ((t ,@(spacious-padding-set-face-box-padding 'tab-bar-tab-inactive 'tab-bar))))
     `(tab-line-tab ((t ,@(spacious-padding-set-face-box-padding 'tab-line-tab 'tab-line))))
     `(tab-line-tab-inactive ((t ,@(spacious-padding-set-face-box-padding 'tab-line-tab-inactive 'tab-line))))
     `(tab-line-tab-active ((t ,@(spacious-padding-set-face-box-padding 'tab-line-tab-active 'tab-line))))
     `(tab-line-tab-current ((t ,@(spacious-padding-set-face-box-padding 'tab-line-tab-current 'tab-line))))
     `(vertical-border ((t :background ,bg-main :foreground ,bg-main)))
     `(,@(spacious-padding-set-window-divider 'window-divider bg-main))
     `(,@(spacious-padding-set-window-divider 'window-divider-first-pixel bg-main))
     `(,@(spacious-padding-set-window-divider 'window-divider-last-pixel bg-main)))))
