;;; reconfig.el -*- lexical-binding: t; -*-

(progn :misc

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

       (auto-dim-other-buffers-mode nil))

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

(progn :lisp

       (defun pb/lisp-escape-insert-mode ()
         (interactive)
         (evil-normal-state)
         (forward-char)
         (symex-mode-interface))

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
                  (backward-char 2))))

       (use-package cider
         :config
         (setq cider-use-overlays t)
         (setq cider-print-fn 'pprint)
         (setq cider-print-options '(("length" 50) ("right-margin" 70))))

       (use-package symex

         :custom
         (symex-modal-backend 'evil)

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
                 ;("K" . +lookup/documentation)
                 ("s-r" . symex-repl)
                 ("r" . paredit-raise-sexp)
                 ("R" . pb/symex-replace)
                 ("t" . pb/toggle-level-hiding) ;; hs-toggle-hiding
                 ("F" . pb/goto-next-opening-delimiter)
                 ("B" . pb/goto-prev-opening-delimiter)
                 ("T" . pb/toggle-hiding)
                 ("E" . pb/symex-eval-pp-clojure)

                 ("}" . symex-wrap-curly)
                 ("{" . symex-create-curly)

                 ("C-;" . pb/symex-clj-toggle-comment)
                 ("<tab>" . symex-tidy)

                 ("h" . symex-go-backward)
                 ("l" . symex-go-forward)
                 ;("h" . pb/symex-bw)
                 ;("l" . pb/symex-fw)
                 ("C-k" . pb/symex-previous-line)
                 ("C-j" . pb/symex-next-line)
                                        ;("C-k" . symex-descend-branch)
                                        ;("C-j" . symex-climb-branch)

                 ("C" . copilot-hydra/body)
                 ))

         (symex-initialize)


         (setq evil-symex-state-cursor
               '("#B589FE" box))

         (defun symex-eval-clojure ()
           (interactive)

           (cider-eval-last-sexp))

         (general-define-key
          :states 'insert
          :keymaps '(emacs-lisp-mode-map clojure-mode-map)
          [escape] #'pb/lisp-escape-insert-mode
          ;; [return] #'symex-mode-interface
          ";" (lambda () (interactive) (print "please use  M-;"))
          "M-;" (lambda () (interactive) (insert ";"))
          "C-w" #'pb/insert-open-paren
          "C-j" #'symex-mode-interface)

         (general-define-key
          :states 'normal
          :keymaps '(emacs-lisp-mode-map clojure-mode-map)
          "C-w" (lambda () (interactive) (evil-insert 1) (pb/insert-open-paren))
          "C-j" #'symex-mode-interface
          "RET" #'symex-mode-interface)

         (general-define-key
          :states 'normal
          :keymaps '(clojure-mode-map)
          ";" #'pb/symex-clj-toggle-comment)))

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
'(load "~/.doom.d/parts/color-picker/color-picker.el")

;; TOODO

(use-package! copilot
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
(use-package! gpt
  :config
  (setq gpt-openai-key "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")
  (setq gpt-openai-engine "text-davinci-003")
  :bind (:map evil-normal-state-map ("g p" . gpt-dwim)))

;; First install the package:
(use-package flycheck-clj-kondo
  :ensure t)

;; then install the checker as soon as `clojure-mode' is loaded
(use-package clojure-mode
  :ensure t
  :config
  (require 'flycheck-clj-kondo)
  (copilot-mode nil))

(use-package centaur-tabs
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
  ("C-<right>" . centaur-tabs-forward))
