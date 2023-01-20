;;; config/org.el -*- lexical-binding: t; -*-

(use-package org-bullets
  :config
  (setq org-bullets-bullet-list '("⁖")) ; "◉" "○"
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (setq org-modern-star ["◉" "○" "◈" "◇"])) ;; "◉"  "✳"

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
  (require 'org-habit)
  (require 'ol-info)
  (require 'org-protocol-capture-html)

  (setq org-ellipsis "…")
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

  (setq org-tag-alist
        nil
        ;; ((:startgrouptag)
        ;;  ("g1")
        ;;  (:grouptags) ("g1-1") ("g1-2") (:endgrouptag)
        ;;  (:startgrouptag) ("g1-1") (:grouptags) ("g1-1-1") ("g1-1-2") (:endgrouptag)
        ;;  (:startgrouptag) ("g2-1") (:grouptags) ("g2-1-1") ("g2-1-2") (:endgrouptag))
        )

  (setq org-tag-persistent-alist
        '((:startgroup . "emacs") ("org" . ?o) ("doom" . ?d) ("elisp" . ?e) (:endgroup . nil)
          (:startgroup . "work") ("clojure" . ?c) ("prolog" . ?p) (:endgroup . nil)
          ("music" . ?m)
          ("organisation" . ?O)))

  (setq org-roam-capture-templates

        '(("d" "default" plain "%?" :target
           (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)
          ("s" "simple" plain "%?" :target
           (file+head "${title}.org" "#+title: ${title}\n")
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

(use-package org-gtd
  :after org
  :bind
  ()
  :config
  (setq org-gtd-directory "~/org/gtd")
  ;; avoid to delete windows when processing inbox
  (defun org-gtd-process-inbox ()
    "Process the GTD inbox."
    (interactive)
    (set-buffer (org-gtd--inbox-file))
    (display-buffer-same-window (org-gtd--inbox-file) '())
    ;; (delete-other-windows)

    (org-gtd-process-mode t)

    (condition-case err
        (progn
          (widen)
          (goto-char (point-min))
          (org-next-visible-heading 1)
          (org-back-to-heading)
          (org-narrow-to-subtree))
      (user-error (org-gtd--stop-processing)))))

(use-package org-modern
  :after org
  :config
  (add-hook 'org-mode-hook #'org-modern-mode)
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda))

;; (use-package svg-tag-mode)
;; (setq svg-tag-tags
;;       '(("TOODO" . ((lambda (tag) (svg-tag-make "TODO" :radius 5 :foreground "#3CB371"))))))

(provide 'org-config)
