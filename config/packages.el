;; -*- no-byte-compile: t; -*-
;;; config/packages.el

(use-package company
  :config
  (set-company-backend! 'org-mode nil)
  (setq company-idle-delay 0.3))

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

    ;; (setq dired-omit-files ...)
    (add-hook 'dired-mode-hook
              (lambda ()
                (dired-omit-mode)
                (dired-hide-details-mode)
                (dired-sort-toggle-or-edit)))))

(use-package dired-sidebar)

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
  :after (doom-themes)
  :config
  ;; tweak appearance
  (add-hook 'ibuffer-mode-hook (lambda () (setq-local line-spacing 6)))
  (setq ibuffer-filter-group-name-face (list :foreground (doom-color 'magenta) :weight 'ultra-bold :height 1.1))
  (setq ibuffer-title-face (list :foreground (doom-blend 'fg 'bg 0.2) :weight 'normal :height 1.1))

  ;; remove some buffers from ibuffer list
  (setq ibuffer-never-show-predicates
        '("^:~/.*" ;; dired sidebar
          "^magit"
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

(use-package gptel
  :config
  ;; this key is set in secrets.el
  (setq gptel-api-key pb/openai-api-key)
  (setq gptel-default-mode 'org-mode)
  ;; bindings
  (setq-default gptel-model "gpt-4")
  (map! :n "g p" #'gptel)
  (map! (:map gptel-mode-map
              :i "C-e" #'gptel-send
              :ni "C-m" #'gptel-menu)))

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
