(print "hello")



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

(add-hook 'lispy-mode-hook #'lispyville-mode)

(use-package lispyville
  :init
  (general-add-hook '(emacs-lisp-mode-hook lisp-mode-hook) #'lispyville-mode)
  :config
  (lispyville-set-key-theme
   '(operators
     c-w
     prettify
     (escape insert)
     (additional-movement normal visual motion))))

;; not working
'(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-application-framework/")
'(require 'eaf)

(use-package org-bullets
  :config
  (setq org-bullets-bullet-list '("⁖")) ; "◉" "○"
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package org
  :commands (org-edit-src-code)
  :bind
  (:map evil-normal-state-map
   ("g e" . org-edit-src-code))
  :config
  (add-to-list 'org-structure-template-alist '("red" . "src red"))
  (setq org-tag-alist '(("emacs" ?e) ("org" . ?o))))

(load-theme 'doom-mine)
(setq doom-font (font-spec :family "Menlo" :size 16))

'(add-to-list 'desktop-path "~/pierrebaille/.doom.d/")
'(desktop-read)
