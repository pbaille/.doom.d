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

'(use-package lispyville
  :init
  (general-add-hook '(emacs-lisp-mode-hook lisp-mode-hook) #'lispyville-mode)
  :config
  (lispyville-set-key-theme
   '(operators
     c-w
     prettify
     (escape insert)
     ; (additional-movement normal visual motion)
     )))

'(use-package evil-lispy
  :init
  (general-add-hook '(emacs-lisp-mode-hook clojure-mode-hook lisp-mode-hook) #'evil-lispy-mode) )

(use-package symex
  :custom
  (symex-modal-backend 'hydra)
  :config
  (symex-initialize)
  (global-set-key (kbd "s-;") 'symex-mode-interface))

(use-package org-bullets
  :config
  (setq org-bullets-bullet-list '("⁖")) ; "◉" "○"
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(map! (:after org
       :map org-mode-map
       :n "J" #'outline-next-visible-heading
       :n "K" #'outline-previous-visible-heading
       :n "S" #'org-insert-structure-template))


(use-package org

  :commands (org-edit-src-code)

  :bind
  (:map evil-normal-state-map
   ("g e" . org-edit-src-code)
   ;; ("g l" . org-down-element)
   )

  :config

  (require 'org-tempo)
  (require 'ol-info)


  (progn :clock-sound

         (setq org-clock-sound "~/.doom.d/bell.wav")

         (defun my/play-sound (orgin-fn sound)
           (cl-destructuring-bind (_ _ file) sound
             (make-process :name (concat "play-sound-" file)
                           :connection-type 'pipe
                           :command `("afplay" ,file))))
         (advice-add 'play-sound :around 'my/play-sound))



  (add-to-list 'org-structure-template-alist '("eli" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("red" . "src red"))
  (add-to-list 'org-structure-template-alist '("clj" . "src clojure"))

  (setq org-tag-persistent-alist

        '((:startgroup . "emacs") ("org" . ?o) ("doom" . ?d) ("elisp" . ?e) (:endgroup . nil)
          (:startgroup . "work") ("clojure" . ?c) ("prolog" . ?p) (:endgroup . nil)
          ("music" . ?m)
          ("organisation" . ?O)))

  (setq org-roam-capture-templates

        '(("d" "default" plain "%?" :target
           (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)

          ("p" "project" plain "%?" :target
           (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category:{title}\n#+TAGS: project\n")
           :unnarrowed t)))

  (setq org-roam-capture-ref-templates
        `(("r" "ref" plain "%?"
           :target (file+head "inbox/${slug}.org"
                              "#+title: ${title}\n#+filetags: r_ex")
           :unnarrowed t
           :immediate-finish t)))

  (setq org-capture-templates

        `(("t" "Personal todo" entry
           (file+headline +org-capture-todo-file "Inbox")
           "* [ ] %?\n%i\n%a" :prepend t)

          ("n" "Personal notes" entry
           (file+headline +org-capture-notes-file "Inbox")
           "* %u %?\n%i\n%a" :prepend t)

          ("j" "Journal" entry
           (file+olp+datetree +org-capture-journal-file)
           "* %U %?\n%i\n%a" :prepend t)

          ("l" "link" entry
           (file+headline "links.org" "Links")
           "* %u %?\n%a\n\n%i" :prepend t)

          ("L" "org-protocol-link" entry
           (file+headline "links.org" "Links")
           ,(concat
             "* [[%:link][%(my-transform-square-brackets-to-round-ones \"%:description\")]] :link:\n"
             ":PROPERTIES:\n"
             ":CREATED: %U\n"
             ":END:\n"
             "\n"
             "#+BEGIN_QUOTE\n"
             "%i\n"
             "#+END_QUOTE\n")
           :immediate-finish t
           :empty-lines 1
           :prepend nil)



          ("p" "Templates for projects")

          ("pt" "Project-local todo" entry
           (file+headline +org-capture-project-todo-file "Inbox")
           "* TODO %?\n%i\n%a" :prepend t)

          ("pn" "Project-local notes" entry
           (file+headline +org-capture-project-notes-file "Inbox")
           "* %U %?\n%i\n%a" :prepend t)

          ("pc" "Project-local changelog" entry
           (file+headline +org-capture-project-changelog-file "Unreleased")
           "* %U %?\n%i\n%a" :prepend t)



          ("o" "Centralized templates for projects")

          ("ot" "Project todo" entry
           #'+org-capture-central-project-todo-file "* TODO %?\n %i\n %a"
           :heading "Tasks" :prepend nil)

          ("on" "Project notes" entry
           #'+org-capture-central-project-notes-file "* %U %?\n %i\n %a"
           :heading "Notes" :prepend t)

          ("oc" "Project changelog" entry
           #'+org-capture-central-project-changelog-file "* %U %?\n %i\n %a"
           :heading "Changelog" :prepend t)

          ("w" "Web site" entry
           (file "")
           "* %a :website:\n\n%U %?\n\n%:initial")
          ))


  (setq org-tag-alist nil))

(load-theme 'doom-horizon-tweaked)
(setq doom-font (font-spec :family "Menlo" :size 16))
'(highlight-parentheses-mode nil)
(turn-off-show-smartparens-mode)
'(global-highlight-parentheses-mode -1)

'(add-to-list 'desktop-path "~/pierrebaille/.doom.d/")
'(desktop-read)

(map! :leader
      "o d" #'dired-jump
      "o D" #'+debugger/start)

(map! "<f12>" #'execute-extended-command)

;(map! :map override "<f14>" #'doom/leader)

'(use-package evil-collection
   :after evil
   :ensure t
   :config
   '(setq evil-collection-mode-list '(lispy))
   (evil-collection-init))

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; not working
'(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-application-framework/")
'(require 'eaf)

;; (setq browse-url-browser-function 'eaf-open-browser)
;; (defalias 'browse-web #'eaf-open-browser)
;; (setq eaf-browser-enable-adblocker "true")
;; (setq eaf-browser-continue-where-left-off t)
;; (setq eaf-browser-default-search-engine "duckduckgo")
;; (setq eaf-browse-blank-page-url "https://duckduckgo.com")
;; (setq eaf-browser-default-zoom "3")
;; (require 'eaf-org)
;; (defun eaf-org-open-file (file &optional link)
;; "An wrapper function on eaf-open'." (eaf-open file)) ;; use emacs-application-framework' to open PDF file: link
;; (add-to-list 'org-file-apps '("\.pdf\'" . eaf-org-open-file))
