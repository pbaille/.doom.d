;;; reconfig.el -*- lexical-binding: t; -*-


(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(setq display-line-numbers-type 'relative)

(progn :theme
       (load-theme 'doom-horizon-tweaked)
       (setq doom-font (font-spec :family "Menlo" :size 16))
       '(highlight-parentheses-mode nil)
       (turn-off-show-smartparens-mode)
       '(global-highlight-parentheses-mode -1))

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

;; symex ---------------------------------------------------

(defun pb/lisp-escape-insert-mode ()
  (interactive)
  (evil-normal-state)
  (symex-mode-interface))

(defun pb/insert-paren ()
  (interactive)
  (execute-kbd-macro (kbd "(")))

(use-package cider
  :config
  (setq cider-use-overlays nil)
  (setq cider-print-fn 'pprint)
  (setq cider-print-options '(("length" 50) ("right-margin" 70))))

(use-package symex

  :custom
  (symex-modal-backend 'evil)

  :bind
  (("s-l" . symex-mode-interface))

  :config
  (symex-initialize)
  ;; (global-set-key (kbd "s-l") 'symex-mode-interface)
  (setq evil-symex-state-cursor '("cyan" box))
  ;; (general-def 'insert emacs-lisp-mode-map
  ;;   [escape] 'pb/lisp-escape-insert-mode)

  (defun symex-eval-clojure ()
    (interactive)
    (cider-pprint-eval-last-sexp))

  (general-define-key
   :states 'insert
   :keymaps '(emacs-lisp-mode-map clojure-mode-map)
   [escape] #'pb/lisp-escape-insert-mode
   "C-w" #'pb/insert-open-paren)

  ;; do not work
  (setq symex--user-evil-keyspec
        '(("C-w" . #'pb/insert-open-paren)))

  ;; (general-def 'normal emacs-lisp-mode-map
  ;;   (kbd "C-f") (lambda ()
  ;;                 (interactive)
  ;;                 (execute-kbd-macro (kbd "("))))
  )

;; load -----------------------------------------------------

(load "~/.doom.d/config/org.el")
(load "~/.doom.d/bindings.el")

(defadvice! enforce-encoding-a (fn &rest args)
  :around #'persp-list-persp-names-in-file
  :around #'persp-load-state-from-file
  :around #'persp-save-state-to-file
  (let ((coding-system-for-read 'utf-8)
        (coding-system-for-write 'utf-8))
    (apply fn args)))
