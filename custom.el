(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ignored-local-variable-values
   '((eval when
      (fboundp 'rainbow-mode)
      (rainbow-mode 1))
     (eval add-hook 'after-save-hook
      (lambda nil
        (if
            (y-or-n-p "Tangle?")
            (org-babel-tangle)))
      nil t)
     (eval add-hook 'after-save-hook
      (lambda nil
        (if
            (y-or-n-p "Reload?")
            (load-file user-init-file)))
      nil t)))
 '(package-selected-packages
   '(auto-dim-other-buffers centaur-tabs metronome origami org-superstar org-bullets ob-clojurescript nameless info-colors command-log-mode autothemer))
 '(warning-suppress-types
   '((after-change-major-mode-hook)
     (evil-symex-state-exit-hook)
     (evil-symex-state-exit-hook)
     (eshell-post-command-hook))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'customize-themes 'disabled nil)
