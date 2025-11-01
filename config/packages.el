;; -*- no-byte-compile: t; lexical-binding: t; -*-
;;; config/packages.el

(use-package evil
  :config
  ;; this is needed to prevent the cursor color to change in unexpected ways:
  ;; https://github.com/emacs-evil/evil/issues/1835#issuecomment-1722500688
  (advice-add 'evil-initialize-state
              :around
              (lambda (orig-fun &rest args)
                (if (or (string-match "*eldoc" (buffer-name))
                        (string-match " *temp*" (buffer-name)))
                    nil
                  (apply orig-fun args)))))

(use-package hideshow
  :config (set-face-attribute '+fold-hideshow-folded-face nil :box nil))

(use-package lsp-mode
  :hook
  (lsp-mode . (lambda ()
                (setq lsp-graphql-target-file-extensions ())))
  :config
  (setq lsp-ui-doc-show-with-mouse nil))

(use-package lsp-modeline)

(use-package spacious-padding
  :config
  (setq spacious-padding-widths
        '(:internal-border-width 15
          :header-line-width 4
          :mode-line-width 6
          :tab-width 4
          :right-divider-width 15
          :scroll-bar-width 8
          :fringe-width 8))
  (spacious-padding-mode 1)
  (remove-hook 'doom-init-ui-hook #'window-divider-mode))

(use-package company
  :config
  (set-company-backend! 'org-mode nil)
  (setq company-idle-delay nil))

(use-package nerd-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package dired
  :commands (dired dired-jump)
  :bind (("C-d" . dired-jump))
  :config
  (progn
    ;; this fix the 'ls does not supports --dired' error
    ;; TODO this may have been needed due to brutal erasing of .local
    ;; should have been fixed by running `doom env` ... to check
    (when (string= system-type "darwin")
      (setq dired-use-ls-dired t
            insert-directory-program "/usr/local/bin/gls"
            dired-listing-switches "-aBhl --group-directories-first"))

    (setq delete-by-moving-to-trash t)

    '(setq dired-omit-files "\\|^\\.[^.]")
    '(setq dired-omit-files
           (concat dired-omit-files "\\|^\\.[^.]"))

    (add-hook 'dired-mode-hook
              (lambda ()
                (dired-omit-mode 1)
                (dired-hide-details-mode 1)
                (nerd-icons-dired-mode 1)
                ;; (setq nerd-icons-color-icons t)
                (dired-sort-toggle-or-edit)))

    (add-hook 'dired-after-readin-hook
              (lambda ()
                (let ((inhibit-read-only t))
                  (save-excursion
                    (goto-char (point-min))
                    (when (re-search-forward "^  \\(/Users/pierrebaille\\)\\(.*:\\)$" nil t)
                      (replace-match "  ~\\2" nil nil))
                    ))))))

(use-package nerd-icons-dired
  :preface
  (defun my/nerd-icons-icon-for-file (file)
    (nerd-icons-icon-for-file file :face (list :foreground (doom-blend "#535"
                                                                       (doom-lighten 'base3 0.3)
                                                                       0.4))))

  (defun my/nerd-icons-icon-for-dir (dir)
    (nerd-icons-icon-for-dir dir ;:height 0.9
                             :face '(:foreground "#575")))

  :custom
  (nerd-icons-dired-file-icon-function #'my/nerd-icons-icon-for-file)
  (nerd-icons-dired-dir-icon-function #'my/nerd-icons-icon-for-dir))

(use-package dirvish
  :config
  (dirvish-override-dired-mode -1)
  (setq dirvish-hide-details t))

(use-package embark
  :config (setq embark-quit-after-action nil))

(use-package dired-sidebar
  :config
  (setq dired-sidebar-theme 'nerd-icons))

(use-package! vterm
  :config
  (add-hook 'vterm-mode-hook
            (lambda ()
              (hide-mode-line-mode -1)
              (doom-modeline-mode 1))
            10000000))

(use-package magit
  :config
  (setq git-commit-style-convention-checks
        (remove 'overlong-summary-line git-commit-style-convention-checks)))


(use-package vertico
  :config
  (setq vertico-buffer-display-action '(display-buffer-below-selected) )
  (setq vertico-buffer-mode t))

(use-package consult
  :config
  (defun consult--buffer-sort-file-first (buffers)
    (sort buffers (lambda (a b)
                    (let ((file-a (buffer-file-name a))
                          (file-b (buffer-file-name b)))
                      (if file-a
                          (if file-b
                              ;; If both buffers visit files, sort alphabetically.
                              (< (or (seq-position buffers a) most-positive-fixnum)
                                 (or (seq-position buffers b) most-positive-fixnum))
                            ;; Buffer A visits a file, but buffer B doesn't.
                            t)
                        ;; Buffer A doesn't visit a file.
                        (if file-b
                            ;; Buffer B visits a file, so buffer A goes at the end.
                            nil
                          ;; Neither buffer visits a file, so sort alphabetically.
                          (string-lessp (buffer-name a) (buffer-name b))))))))

  (setq consult-narrow-key "/")

  (setq consult-project-function (lambda (_) (doom-project-root)))

  (setq consult--source-buffer
        `(:name     "Buffer"
          :narrow   (?b . "Buffer")
          ;;:hidden   t
          :history  buffer-name-history
          :category buffer
          :state    ,#'consult--buffer-state
          ;; The buffer source must come first, because the `consult--multi' completion
          ;; system gives the candidates from the first source which returns non-nil
          ;; as the "initial input". For `consult-buffer' this should be the `consult--source-buffer'.
          :default  (lambda () (buffer-name (consult--buffer-query)))
          :items
          ,(lambda () (consult--buffer-query :sort 'file-first :as #'buffer-name)))))

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

  (setq-default flycheck-disabled-checkers
                '(emacs-lisp-package emacs-lisp-checkdoc))

  (flycheck-define-error-level 'error
    :overlay-category 'flycheck-error-overlay
    :fringe-bitmap 'flycheck-fringe-bitmap-ball
    :fringe-face 'flycheck-fringe-error)

  (flycheck-define-error-level 'warning
    :overlay-category 'flycheck-warning-overlay
    :fringe-bitmap 'flycheck-fringe-bitmap-ball
    :fringe-face 'flycheck-fringe-warning))

(use-package ibuffer
  :after (doom-themes)
  :config
  ;; tweak appearance

  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (setq-local line-spacing 6)))
  (setq ibuffer-filter-group-name-face
        (list :foreground (doom-color 'red) :weight 'ultra-bold :height 1.1))
  (setq ibuffer-title-face (list :foreground (doom-blend 'fg 'bg 0.2) :weight 'normal :height 1.1))
  (custom-set-faces
   `(nerd-icons-ibuffer-file-face ((t (:foreground ,(doom-blend 'fg 'bg 0.2) :weight normal)))))
  (custom-set-faces
   `(nerd-icons-ibuffer-mode-face ((t (:foreground ,(doom-blend 'fg 'bg 0.6) :weight normal)))))
  (custom-set-faces
   `(nerd-icons-ibuffer-size-face ((t (:foreground ,(doom-blend 'fg 'bg 0.3) :weight normal)))))

  ;; remove some buffers from ibuffer list
  '(setq ibuffer-never-show-predicates
         '("^:~/.*" ;; dired sidebar
           (lambda (x) (with-current-buffer x (eq 'dired-mode major-mode)))))
  (setq ibuffer-never-show-predicates
        nil)

  ;; try to collapse default group by default
  ;; do not work for now
  (setq ibuffer-hidden-filter-groups (list "[ Default ]"))

  (setq ibuffer-fontification-alist
        `((10 buffer-read-only font-lock-constant-face)
          (15 (and buffer-file-name
                   (string-match ibuffer-compressed-file-name-regexp
                                 buffer-file-name))
              font-lock-doc-face)
          (18 (buffer-modified-p) ((t :foreground ,(doom-color 'yellow) :weight bold)))
          (20 (string-match "^\\*" (buffer-name)) ((t (:foreground ,(doom-blend 'fg 'bg 0.5) :weight normal))))
          (25 (and (string-match "^ " (buffer-name))
                   (null buffer-file-name))
              italic)
          (30 (memq major-mode ibuffer-help-buffer-modes) font-lock-comment-face)
          (36 (derived-mode-p 'dired-sidebar-mode) font-lock-comment-face)
          (35 (derived-mode-p 'dired-mode) font-lock-function-name-face)
          (40 (and (boundp 'emacs-lock-mode) emacs-lock-mode) ibuffer-locked-buffer))))

(use-package ibuffer-projectile
  :config
  (setq ibuffer-projectile-prefix ""))

(use-package prettier-js
  :config
  (add-hook 'js2-mode-hook #'prettier-js-mode)
  (add-hook 'typescript-mode-hook #'prettier-js-mode)
  (add-hook 'web-mode-hook #'prettier-js-mode))

(use-package hl-line
  :config
  ;; Option 1: Call the function to disable it
  (global-hl-line-mode -1)

  ;; Option 2: If you want to disable it completely
  ;; and prevent it from being enabled by other packages
  (setq global-hl-line-sticky-flag nil)

  ;; Option 3: If you want to control when it's enabled
  '(add-hook 'prog-mode-hook #'hl-line-mode))

(use-package yasnippet
  :config
  (setq yas-global-mode nil)
  (add-hook 'emacs-lisp-mode-hook (lambda () (yas-minor-mode -1))))

(use-package gptel
  :config
  ;; this key is set in secrets.el
  (setq gptel-api-key pb/openai-api-key)
  (setq gptel-default-mode 'org-mode)
  (setq gptel-use-header-line nil)
  (setq gptel-use-tools nil)
  (setq gptel-include-tool-results t)
  (setq gptel-cache t)
  (setq gptel-log-level 'info)
  ;; bindings
  ;; the only way I found for redefining the broken RET key of the menu to C-RET
  ;; the problem is that gptel-menu is autoloaded,
  ;; it has to be called once before the rebinding is possible.
  (defun pb-gptel-change-return-key-in-menu ()
    (transient-suffix-put 'gptel-menu (kbd "RET") :key "C-<return>")
    (advice-remove #'gptel-menu #'pb-gptel-change-return-key-in-menu))
  (advice-add #'gptel-menu :after #'pb-gptel-change-return-key-in-menu)

  (setq-default gptel-model
                "claude-sonnet-4-20250514")

  (setq gptel-backend
        (gptel-make-anthropic "Claude"          ;Any name you want
          :stream t                             ;Streaming responses
          :key pb/claude-api-key))

  (gptel-make-gemini "Gemini"
    :key pb/gemini-api-key :stream t))

'(use-package tide
   :config
   (setq tide-server-max-response-length 1000000))

(use-package mindstream
  :config
  (mindstream-mode)
  (setq mindstream-path "/Users/pierrebaille/.mindstream/anon")
  (setq mindstream-template-path "/Users/pierrebaille/.mindstream/templates"))

(progn :edi-prolog

                                        ;(load-file "/Users/pierrebaille/.doom.d/packages/prolog.el")
       (load-file "/Users/pierrebaille/.doom.d/packages/ediprolog.el")

       (add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode))

       (setq ediprolog-system 'swi)

       (setq ediprolog-program "/opt/local/bin/swipl")

       (map! (:map prolog-mode-map
              :n "e" #'ediprolog-dwim
              :n "SPC m k" #'ediprolog-remove-interactions)))

(use-package ligature
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  (ligature-set-ligatures 'elisp-mode '("==" ">=" "<=" "->" "->>"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (ligature-set-ligatures 'org-mode-hook '())
  (global-ligature-mode t))

(use-package gleam-ts-mode
  :mode (rx ".gleam" eos))

(use-package claude-code :ensure t
  :vc (:url "https://github.com/stevemolitor/claude-code.el" :rev :newest)

  :config
  (claude-code-mode)
  (setq claude-code-terminal-backend 'vterm)

  :bind-keymap
  ("s-a" . claude-code-command-map))

'(use-package clay)

(use-package eca
  :config
  (setq eca-chat-use-side-window nil))
