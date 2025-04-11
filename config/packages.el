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
                (setq lsp-graphql-target-file-extensions ()))))

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

'(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

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
                ;; (setq dirvish-hide-details t)
                ;; (dired-hide-details-mode 1)
                ;; (diredfl-mode nil)
                (dired-omit-mode)
                ;; (nerd-icons-dired-mode)
                (dired-sort-toggle-or-edit)))))

(use-package dirvish
  :config
  (setq dirvish-hide-details t))

(use-package embark
  :config (setq embark-quit-after-action nil))

(use-package dired-sidebar
  :config
  (add-hook 'dired-sidebar-mode-hook
            'nerd-icons-dired-mode))

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
  (setq ibuffer-filter-group-name-face (list :foreground (doom-color 'magenta) :weight 'ultra-bold :height 1.1))
  (setq ibuffer-title-face (list :foreground (doom-blend 'fg 'bg 0.2) :weight 'normal :height 1.1))

  ;; remove some buffers from ibuffer list
  (setq ibuffer-never-show-predicates
        '("^:~/.*" ;; dired sidebar
          (lambda (x) (with-current-buffer x (eq 'dired-mode major-mode)))))

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
          (20 (string-match "^\\*" (buffer-name)) font-lock-keyword-face)
          (25 (and (string-match "^ " (buffer-name))
                   (null buffer-file-name))
              italic)
          (30 (memq major-mode ibuffer-help-buffer-modes) font-lock-comment-face)
          (35 (derived-mode-p 'dired-mode) font-lock-function-name-face)
          (40 (and (boundp 'emacs-lock-mode) emacs-lock-mode) ibuffer-locked-buffer))))

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
  (setq yas-global-mode nil))

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
                "claude-3-7-sonnet-20250219")

  (setq gptel-backend
        (gptel-make-anthropic "Claude"          ;Any name you want
          :stream t                             ;Streaming responses
          :key pb/claude-api-key))

  (gptel-make-gemini "Gemini"
    :key pb/gemini-api-key :stream t))

(use-package tide
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
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
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






















'(use-package codeium
   ;; if you use straight
   ;; :straight '(:type git :host github :repo "Exafunction/codeium.el")
   ;; otherwise, make sure that the codeium.el file is on load-path

   :init
   ;; use globally
   (add-to-list 'completion-at-point-functions #'codeium-completion-at-point)
   ;; or on a hook
   ;; (add-hook 'python-mode-hook
   ;;     (lambda ()
   ;;         (setq-local completion-at-point-functions '(codeium-completion-at-point))))

   ;; if you want multiple completion backends, use cape (https://github.com/minad/cape):
   ;; (add-hook 'python-mode-hook
   ;;     (lambda ()
   ;;         (setq-local completion-at-point-functions
   ;;             (list (cape-super-capf #'codeium-completion-at-point #'lsp-completion-at-point)))))
   ;; an async company-backend is coming soon!

   ;; codeium-completion-at-point is autoloaded, but you can
   ;; optionally set a timer, which might speed up things as the
   ;; codeium local language server takes ~0.2s to start up
   ;; (add-hook 'emacs-startup-hook
   ;;  (lambda () (run-with-timer 0.1 nil #'codeium-init)))

   ;; :defer t ;; lazy loading, if you want
   :config
   (setq use-dialog-box nil) ;; do not use popup boxes

   ;; if you don't want to use customize to save the api-key
   ;; (setq codeium/metadata/api_key "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx")

   ;; get codeium status in the modeline
   (setq codeium-mode-line-enable
         (lambda (api) (not (memq api '(CancelRequest Heartbeat AcceptCompletion)))))
   (add-to-list 'mode-line-format '(:eval (car-safe codeium-mode-line)) t)
   ;; alternatively for a more extensive mode-line
   ;; (add-to-list 'mode-line-format '(-50 "" codeium-mode-line) t)

   ;; use M-x codeium-diagnose to see apis/fields that would be sent to the local language server
   (setq codeium-api-enabled
         (lambda (api)
           (memq api '(GetCompletions Heartbeat CancelRequest GetAuthToken RegisterUser auth-redirect AcceptCompletion))))
   ;; you can also set a config for a single buffer like this:
   ;; (add-hook 'python-mode-hook
   ;;     (lambda ()
   ;;         (setq-local codeium/editor_options/tab_size 4)))

   ;; You can overwrite all the codeium configs!
   ;; for example, we recommend limiting the string sent to codeium for better performance
   (defun pb-codeium/document/text ()
     (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (min (+ (point) 1000) (point-max))))
   ;; if you change the text, you should also change the cursor_offset
   ;; warning: this is measured by UTF-8 encoded bytes
   (defun pb-codeium/document/cursor_offset ()
     (codeium-utf8-byte-length
      (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (point))))
   (setq codeium/document/text 'pb-codeium/document/text)
   (setq codeium/document/cursor_offset 'pb-codeium/document/cursor_offset))
