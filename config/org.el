;;; config/org.el -*- lexical-binding: t; -*-

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
  (require 'org-protocol-capture-html)

  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)

  (load "~/.doom.d/config/org/templates.el")

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

  (setq org-tag-alist nil)

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
           :immediate-finish t))))

(provide 'org-config)
