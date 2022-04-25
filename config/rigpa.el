;;; rigpa.el -*- lexical-binding: t; -*-


(use-package rigpa

  :after (evil symex)

  :config
  (setq rigpa-mode t)

  ;; temporary workaround for https://github.com/countvajhula/rigpa/issues/9
  (remove-hook 'evil-symex-state-exit-hook #'symex-disable-editing-minor-mode)

  ;; custom config
  (setq rigpa-show-menus nil)

  ;; navigating meta modes
  (global-unset-key (kbd "s-m"))
  (global-set-key (kbd "s-m s-m") 'rigpa-flashback-to-last-tower)
  (global-set-key (kbd "C-<escape>")
                  (lambda ()
                    (interactive)
                    (when (eq rigpa--complex rigpa-meta-complex)
                      (rigpa-exit-mode-mode))
                    (rigpa-enter-tower-mode)))
  (global-set-key (kbd "M-<escape>") 'rigpa-enter-mode-mode)
  (global-set-key (kbd "s-<escape>") 'rigpa-enter-mode-mode)
  (global-set-key (kbd "M-<return>")
                  (lambda ()
                    (interactive)
                    (when (eq rigpa--complex rigpa-meta-complex)
                      (rigpa-enter-selected-level)
                      (let ((ground (rigpa--get-ground-buffer)))
                        (rigpa-exit-mode-mode)
                        (switch-to-buffer ground)))))
  (global-set-key (kbd "s-<return>")
                  (lambda ()
                    (interactive)
                    (when (eq rigpa--complex rigpa-meta-complex)
                      (rigpa-enter-selected-level)
                      (let ((ground (rigpa--get-ground-buffer)))
                        (rigpa-exit-mode-mode)
                        (switch-to-buffer ground)))))
  (global-set-key (kbd "C-<return>")
                  (lambda ()
                    (interactive)
                    (when (eq rigpa--complex rigpa-meta-tower-complex)
                      (rigpa-exit-tower-mode)
                      (rigpa-enter-mode-mode))))

  ;; indexed entry to various modes
  (global-set-key (kbd "s-n") 'evil-normal-state)
  (global-set-key (kbd "s-y")        ; symex mode
                  (lambda ()
                    (interactive)
                    (rigpa-enter-mode "symex")))
  (global-set-key (kbd "s-;") (kbd "s-y"))
  (global-set-key (kbd "s-w")        ; window mode
                  (lambda ()
                    (interactive)
                    (rigpa-enter-mode "window")))
  (global-set-key (kbd "s-v")        ; view mode
                  (lambda ()
                    (interactive)
                    (rigpa-enter-mode "view")))
  (global-set-key (kbd "s-x")       ; char mode
                  (lambda ()
                    (interactive)
                    (rigpa-enter-mode "char")))
  (global-set-key (kbd "s-a")        ; activity mode
                  (lambda ()
                    (interactive)
                    (rigpa-enter-mode "activity")))
  (global-set-key (kbd "s-z")        ; text mode
                  (lambda ()
                    (interactive)
                    (rigpa-enter-mode "text")))
  (global-set-key (kbd "s-g")        ; history mode
                  (lambda ()
                    (interactive)
                    (rigpa-enter-mode "history")))
  (global-set-key (kbd "s-i")        ; system mode
                  (lambda ()
                    (interactive)
                    (rigpa-enter-mode "system")))
  (global-set-key (kbd "s-b")        ; buffer mode
                  (lambda ()
                    (interactive)
                    (rigpa-enter-mode "buffer")))
  (global-set-key (kbd "s-f")        ; file mode
                  (lambda ()
                    (interactive)
                    (rigpa-enter-mode "file")))
  (global-set-key (kbd "s-t")        ; tab mode
                  (lambda ()
                    (interactive)
                    (rigpa-enter-mode "tab")))
  (global-set-key (kbd "s-l")        ; line mode
                  (lambda ()
                    (interactive)
                    (rigpa-enter-mode "line")))
  (global-set-key (kbd "s-e")        ; application mode
                  (lambda ()
                    (interactive)
                    (rigpa-enter-mode "application")))
  (global-set-key (kbd "s-r")        ; word mode
                  (lambda ()
                    (interactive)
                    (rigpa-enter-mode "word"))))
