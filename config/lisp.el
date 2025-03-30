;;; config/lisp.el -*- lexical-binding: t; -*-

(require 'pb-fold)

(defvar pb-config_lisp-modes
  '(emacs-lisp-mode-map clojure-mode-map fennel-mode-map scheme-mode-map racket-mode-map))

;; TODO split this between relevant packages and combine together later
(define-abbrev-table 'emacs-lisp-mode-abbrev-table
  '(("wcb" "with-current-buffer" nil 1)
    ("lam" "lambda" nil 1)

    ("pif" "pb_if" nil 1)
    ("plet" "pb_let" nil 1)
    ("fn" "pb_fn" nil 1)
    ("defn" "pb_defun" nil 1)

    ("kget" "km_get" nil 1)
    ("kput" "km_put" nil 1)
    ("kupd" "km_upd" nil 1)
    ("pbcol" "pb-color" nil 1)))

(add-hook 'emacs-lisp-mode-hook #'abbrev-mode)

(use-package geiser
  :config
  (setq geiser-active-implementations '(chez guile chicken mit chibi gambit))
  (add-hook 'scheme-mode-hook 'geiser-mode)
  (setq geiser-default-implementation 'chez)
  (setq geiser-chez-binary "chez")
  (setq geiser-racket-binary "/usr/local/bin/racket")
  (setq geiser-racket-minimum-version "8.12"))

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
         (setq cider-repl-reuse-dead-buffers nil)
         (setq cider-show-error-buffer nil)
         (setq cider-repl-display-in-current-window t)
         (setq cider-result-buffer "pb-cider_results")
         (setq cider-use-overlays t)
         (setq cider-print-fn 'pprint)
         (setq cider-print-options '(("length" 50) ("right-margin" 70)))
         (setq cider-eldoc-display-for-symbol-at-point nil)
         (setq evil-shift-round nil
               evil-shift-width 1)
         (add-hook 'cider-repl-mode-hook #'pb-cider_kill-dead-buffers))

       (use-package flycheck-clj-kondo)

       '(use-package re-jump)

       (use-package clojure-mode
         :config
         (require 'flycheck-clj-kondo)
         (add-hook 'clojure-mode-hook 'lsp)
         (setq lsp-lens-enable nil)
         (setq lsp-enable-symbol-highlighting nil)
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

  :hook (symex-mode . hs-minor-mode)
  :custom
  (symex-modal-backend 'evil)
  (symex-highlight-p t)
  (symex-quote-prefix-list (list "'" "`"))
  (symex-unquote-prefix-list (list "," ",@" "~" "~@" "#'" "#_" "#" "#\\?"))

  :bind
  (("s-l" . symex-mode-interface))

  :config
  (setq symex-refocus-p nil)
  (setq symex--user-evil-keyspec

        ;; revert k and j in symex to be more intuitive
        ;; by default j goes to the root of the tree, but the root of a symex are higher in the buffer...

        '(("s-r" . symex-repl)
          ("s-R" . query-replace)
                                        ; ("C-d" . +lookup/documentation )
          ("g d" . pb-symex_lookup-definition)
          ("g r" . pb-symex_lookup-references)
          ("g h" . recenter)

          ;; nav
          ("M-k" . scroll-up-with-cursor) ;; symex-goto-lowest
          ("M-j" . scroll-down-with-cursor) ;; symex-goto-highest
          ("h" . symex-go-backward)
          ("l" . symex-go-forward)
          ("j" . pb-symex_go-up)
          ("k" . symex-go-down)

          ;; move current symex
          ("J" . symex-join-lines ;; pb-symex_wrap
           )
          ("K" . pb-symex_raise)

          ;; edit
          ("r" . paredit-raise-sexp)
          ("x" . symex-delete)
          ("u" . pb-symex_undo)
          ("R" . pb-symex_replace)
          ("-" . sp-unwrap-sexp)
          ("s" . sp-unwrap-sexp)
          ("x" . pb-symex_delete)

          ("}" . symex-wrap-curly)
          ("{" . symex-create-curly)

          ("C-S-r" . pb-symex_ring-replace)
          ("C-p" . pb-symex_ring-append)
          ("C-S-p" . pb-symex_ring-prepend)
          ("C-;" . embark-act)

          ("M-C-j" . symex-insert-newline)
          ("M-C-k" . symex-join-lines-backwards)

          ;; llm
          ("q r" . pb-gptel/current-symex-request)
          ("q f" . pb-gptel/fill-holes)
          ("q e" . pb-gptel/current-symex-chat)
          ("q b" . pb-gptel/current-buffer-chat)

          ;; non structural nav
          ("F" . pb-misc_goto-next-opening-delimiter)
          ("B" . pb-misc_goto-prev-opening-delimiter)
          ;; ("h" . pb-symex_bw)
          ;; ("l" . pb-symex_fw)
          ("C-k" . pb-symex_previous-line) ;; replace symex-descend-branch
          ("C-j" . pb-symex_next-line) ;; replace symex-climb-branch
          ("C-l" . evil-forward-char)
          ("C-h" . evil-backward-char)
          ("g j" . evil-scroll-line-to-top)
          ("g k" . evil-scroll-line-to-bottom)
          ("C-S-j" . evil-scroll-line-up)
          ("C-S-k" . evil-scroll-line-down)


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
          ("M" . +lookup/documentation)
          ("E" . symex-eval-print)
          ("C-e" . symex-evaluate-pretty)
          ("M-l" . symex-run)
          (";" . pb-symex_toggle-comment)))

  (symex-initialize)

  (add-hook 'evil-symex-state-entry-hook
            (lambda () (hl-line-mode -1)))

  (add-hook 'evil-symex-state-exit-hook
            (lambda () (hl-line-mode 1)))

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
   ";" (lambda () (interactive) (insert "-"))
   "M-;" (lambda () (interactive) (insert ";"))
   "C-y" #'racket-insert-lambda
   "C-w" #'pb-misc_insert-open-paren
   )

  (general-define-key
   :states 'normal
   :keymaps pb-config_lisp-modes
   [mouse-1] #'pb-symex_click
   [mouse-3] (lambda (event) (interactive "e") (posn-set-point (event-end event)) (evil-insert-state))
   ;; "C-w" (lambda () (interactive) (evil-insert 1) (pb-misc_insert-open-paren))
   "RET" #'symex-mode-interface)

  (general-define-key
   :states 'normal
   :keymaps '(clojure-mode-map)
   "g :" #'re-frame-jump-to-reg)

  (defface doom-modeline-evil-symex-state
    '((t (:inherit doom-modeline-info)))
    "Face for the symex state tag in evil indicator."
    :group 'doom-modeline-faces)

  (defun pb-doom-modeline--modal-icon (f text face help-echo &optional icon unicode)
    "Advice around `doom-modeline--modal-icon'."
    (if (evil-symex-state-p)
        (funcall f
                 (let ((tag (evil-state-property evil-state :tag t)))
                   (if (stringp tag) tag (funcall tag)))
                 'doom-modeline-evil-symex-state
                 (evil-state-property evil-state :name t)
                 "nf-md-alpha_s_circle"
                 "🅢")
      (funcall f text face help-echo icon unicode)))

  (advice-add 'doom-modeline--modal-icon
              :around
              #'pb-doom-modeline--modal-icon))
