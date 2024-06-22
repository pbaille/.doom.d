;;; pb/xp/ef-themes-tweaks.el -*- lexical-binding: t; -*-

(defun pb-ef-themes-custom-faces_light ()
  "My customizations on top of the Ef themes.
This function is added to the `ef-themes-post-load-hook'."
  (ef-themes-with-colors
    (custom-set-faces
     `(hl-line ((,c :background ,(doom-lighten bg-removed-faint 0.4))))
     `(symex--current-node-face ((,c :background ,(doom-lighten bg-removed-faint 0.3))))
     `(ibuffer-title-face ((,c :foreground ,red)))
     `(ibuffer-filter-group-name-face ((,c :foreground ,red)))
     `(show-paren-match ((,c :background ,bg-dim :foreground ,fg-intense)))
     `(diredfl-dir-heading ((,c :foreground ,fg-alt :box (:line-width 8 :color ,bg-main)))))))

(defun pb-ef-themes-custom-faces ()
  "My customizations on top of the Ef themes.
This function is added to the `ef-themes-post-load-hook'."
  (ef-themes-with-colors
    (custom-set-faces
     `(hl-line ((,c :background ,bg-alt)))
     `(symex--current-node-face ((,c :background ,bg-alt)))
     `(ibuffer-title-face ((,c :foreground ,red)))
     `(ibuffer-filter-group-name-face ((,c :foreground ,red)))
     `(show-paren-match ((,c :background ,bg-dim :foreground ,fg-intense)))
     `(diredfl-dir-heading ((,c :foreground ,fg-alt :box (:line-width 8 :color ,bg-main))))
     )))

(ef-themes-select 'ef-summer)
(ef-themes-select 'ef-day)
(ef-themes-select 'ef-dream)
(ef-themes-select 'ef-rosa)
(ef-themes-select 'ef-rosa)
(ef-themes--list-known-themes)
(add-to-list 'custom-theme-load-path "/Users/pierrebaille/.doom.d/themes/")
(ef-themes-select (nth (random (length ef-themes-light-themes)) ef-themes-light-themes))

(ef-themes-select (nth (random (length ef-themes-dark-themes)) ef-themes-dark-themes))

(setq ef-themes-to-toggle '(ef-summer ef-rosa ef-dream))

(ef-themes-select 'pb-ef-dream)
(load-theme 'pb-ef-dream
            :no-confirm)

;; Using the hook lets our changes persist when we use the commands
;; `ef-themes-toggle', `ef-themes-select', and `ef-themes-load-random'.
(add-hook 'ef-themes-post-load-hook #'pb-ef-themes-custom-faces)

(defun pb-ef-themes-ibuffer-overides ()
  ""
  (ef-themes-with-colors
    (setq ibuffer-filter-group-name-face (list :foreground fg-alt :weight 'ultra-bold :height 1.1))
    (setq ibuffer-title-face (list :foreground (doom-blend fg-main bg-main 0.3) :weight 'normal :height 1.1))))

(add-hook 'ef-themes-post-load-hook
          #'pb-ef-themes-ibuffer-overides)
(remove-hook 'ef-themes-post-load-hook
             nil)

(set-face-attribute 'font-lock-comment-face nil :foreground "gray50")
(set-face-attribute 'font-lock-doc-face nil :foreground "gray50")
(ef-themes-with-colors
  (set-face-attribute 'default nil :foreground (doom-darken fg-main 0.1))
  (set-face-attribute 'font-lock-builtin-face nil :weight 'semibold :foreground blue-faint))
