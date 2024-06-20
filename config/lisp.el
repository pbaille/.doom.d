;;; config/lisp.el -*- lexical-binding: t; -*-

(require 'pb-fold)

(defvar pb-config_lisp-modes
  '(emacs-lisp-mode-map clojure-mode-map fennel-mode-map scheme-mode-map racket-mode-map))

(use-package geiser
  :config
  (setq geiser-active-implementations '(chez guile chicken mit chibi gambit))
  (add-hook 'scheme-mode-hook 'geiser-mode)
  (setq geiser-default-implementation 'chez)
  (setq geiser-chez-binary "chez")
                                        ;(setq geiser-racket-binary "/usr/local/bin/racket")
                                        ;(setq geiser-racket-minimum-version "8.12")
  )

(progn :racket
       (setq racket-program "/usr/local/bin/racket")
       (use-package racket-mode
         :config
         (add-to-list 'auto-mode-alist '("\\.rkt$" . racket-mode))
         (add-hook 'racket-mode-hook
                   (lambda ()
                     (racket-xp-mode)))))
(progn :clojure

       (use-package cider
         :config
         (setq cider-show-error-buffer nil)
         (setq cider-use-overlays t)
         (setq cider-print-fn 'pprint)
         (setq cider-print-options '(("length" 50) ("right-margin" 70)))
         (setq cider-eldoc-display-for-symbol-at-point nil)
         (setq evil-shift-round nil
               evil-shift-width 1))

       (use-package flycheck-clj-kondo)

       '(use-package re-jump)

       (use-package clojure-mode
         :config
         (require 'flycheck-clj-kondo)
         (add-hook 'clojure-mode-hook 'lsp)
         (setq lsp-headerline-breadcrumb-enable nil)
         '(copilot-mode nil)))

(use-package fennel-mode

  :bind
  (("s-r" . pb-fennel_fennel-repl))
  :config
  (setq fennel-mode-switch-to-repl-after-reload nil)
  '(set-popup-rules!
    '(("^\\*Fennel REPL" :quit nil :ttl nil))))

(use-package reapl-mode
  :config
  (require 'reapl-symex)
  '(set-popup-rules!
    '(("^\\*reapl-evaluation\\*" :quit nil :ttl nil)))
  (set-lookup-handlers! '(reapl-mode)
    :documentation #'reapl-mode_doc-symbol-at-point)
  (map! (:map company-active-map
              "C-h" #'reapl-mode_request-doc-for-completion-candidate)))

(use-package symex

  :custom
  (symex-modal-backend 'evil)
  (symex-highlight-p t)

  :bind
  (("s-l" . symex-mode-interface))

  :config
  (setq symex--user-evil-keyspec

        ;; revert k and j in symex to be more intuitive
        ;; by default j goes to the root of the tree, but the root of a symex are higher in the buffer...

        '(("s-r" . symex-repl)
          ("d" . dired-jump)
          ("C-d" . +lookup/documentation )

          ;; nav
          ("M-k" . symex-goto-lowest)
          ("M-j" . symex-goto-highest)
          ("j" . pb-symex_go-up)
          ("k" . symex-go-down)
          ("J" . symex-join-lines)
          ("K" . pb-symex_go-down-folding)

          ;; edit
          ("r" . paredit-raise-sexp)
          ("x" . symex-delete)
          ("R" . pb-symex_replace)
          ("-" . sp-unwrap-sexp)

          ("}" . symex-wrap-curly)
          ("{" . symex-create-curly)

          ;; non structural nav
          ("F" . pb-misc_goto-next-opening-delimiter)
          ("B" . pb-misc_goto-prev-opening-delimiter)
          ("h" . symex-go-backward)
          ("l" . symex-go-forward)
          ;; ("h" . pb-symex_bw)
          ;; ("l" . pb-symex_fw)
          ("C-k" . pb-symex_previous-line) ;; replace symex-descend-branch
          ("C-j" . pb-symex_next-line) ;; replace symex-climb-branch

          ;; indent, tidy
          ("<tab>" . pb-sexpr_indent)
          ("<backtab>" . symex-tidy)

          ;; folding
          ("T" . pb-fold_toggle-semi-fold) ;; hs-toggle-hiding
          ("t" . pb-misc_toggle-hiding)
          ("C-t" . hs-hide-level)

          ;; shift symex maintaining indentation
          ("C->" . pb-sexpr_shift-expression-right)
          ("C-<" . pb-sexpr_shift-expression-left)
          ("s->" . pb-sexpr_shift-expressions-right)
          ("s-<" . pb-sexpr_shift-expressions-left)

          ;; mark a symex staying in symex mode
          ("C-SPC" . pb-symex_mark)

          ;; clojure specific TO MOVE
          ("M" . pb-symex_cider-macroexpand) ; +lookup/documentation
          ("E" . pb-symex_eval-pp-clojure)
          ("C-;" . pb-symex_clj-toggle-comment)))

  (symex-initialize)

  (setq evil-symex-state-cursor
        '(box "#2bfafa"))

  (defun symex-eval-clojure ()
    (interactive)
    (cider-eval-last-sexp))

  (general-define-key
   :states 'insert
   :keymaps pb-config_lisp-modes
   [escape] #'pb-symex_escape-insert-mode
   [mouse-1] #'pb-symex_click
   ; ";" (lambda () (interactive) (print "please use  M-;"))
   ; "M-;" (lambda () (interactive) (insert ";"))
   "C-y" #'racket-insert-lambda
   "C-w" #'pb-misc_insert-open-paren)

  (general-define-key
   :states 'normal
   :keymaps pb-config_lisp-modes
   [mouse-1] #'pb-symex_click
   [mouse-3] (lambda (event) (interactive "e") (posn-set-point (event-end event)) (evil-insert-state))
   "C-w" (lambda () (interactive) (evil-insert 1) (pb-misc_insert-open-paren))
   "RET" #'symex-mode-interface)

  (general-define-key
   :states 'normal
   :keymaps '(clojure-mode-map)
   "g :" #'re-frame-jump-to-reg
   ";" #'pb-symex_clj-toggle-comment))
