;;; config/lisp.el -*- lexical-binding: t; -*-

(require 'my-fold)

(defvar pb/lisp-modes
  '(emacs-lisp-mode-map clojure-mode-map fennel-mode-map))

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

       (use-package re-jump)

       (use-package clojure-mode
         :config
         (require 'flycheck-clj-kondo)
         (add-hook 'clojure-mode-hook 'lsp)
         (setq lsp-headerline-breadcrumb-enable nil)
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

        ;; revert k and j in symex to be more intuitive
        ;; by default j goes to the root of the tree, but the root of a symex are higher in the buffer...

        '(("s-r" . symex-repl)
          ("d" . dired-jump)

          ;; nav
          ("M-k" . symex-goto-lowest)
          ("M-j" . symex-goto-highest)
          ("j" . pb/symex-go-up)
          ("k" . symex-go-down)
          ("J" . symex-join-lines)
          ("K" . pb/symex-go-down-folding)

          ;; edit
          ("r" . paredit-raise-sexp)
          ("x" . symex-delete)
          ("R" . pb/symex-replace)

          ("}" . symex-wrap-curly)
          ("{" . symex-create-curly)

          ;; non structural nav
          ("F" . pb/goto-next-opening-delimiter)
          ("B" . pb/goto-prev-opening-delimiter)
          ("h" . symex-go-backward)
          ("l" . symex-go-forward)
          ;; ("h" . pb/symex-bw)
          ;; ("l" . pb/symex-fw)
          ("C-k" . pb/symex-previous-line) ;; replace symex-descend-branch
          ("C-j" . pb/symex-next-line) ;; replace symex-climb-branch

          ;; indent, tidy
          ("<tab>" . pb/indent-sexpr)
          ("<backtab>" . symex-tidy)

          ;; folding
          ("T" . pb/toggle-semi-fold) ;; hs-toggle-hiding
          ("t" . pb/toggle-hiding)
          ("C-t" . hs-hide-level)

          ;; shift symex maintaining indentation
          ("C->" . pb/shift-expression-right)
          ("C-<" . pb/shift-expression-left)
          ("s->" . pb/shift-expressions-right)
          ("s-<" . pb/shift-expressions-left)

          ;; mark a symex staying in symex mode
          ("C-SPC" . pb/symex-mark)

          ;; clojure specific TO MOVE
          ("M" . pb/symex-cider-macroexpand) ; +lookup/documentation
          ("E" . pb/symex-eval-pp-clojure)
          ("C-;" . pb/symex-clj-toggle-comment)))

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
   ";" (lambda () (interactive) (print "please use  M-;"))
   "M-;" (lambda () (interactive) (insert ";"))
   "C-w" #'pb/insert-open-paren)

  (general-define-key
   :states 'normal
   :keymaps pb/lisp-modes
   [mouse-1] #'pb/symex-click
   [mouse-3] (lambda (event) (interactive "e") (posn-set-point (event-end event)) (evil-insert-state))
   "C-w" (lambda () (interactive) (evil-insert 1) (pb/insert-open-paren))
   "RET" #'symex-mode-interface)

  (general-define-key
   :states 'normal
   :keymaps '(clojure-mode-map)
   "g :" #'re-frame-jump-to-reg
   ";" #'pb/symex-clj-toggle-comment))
