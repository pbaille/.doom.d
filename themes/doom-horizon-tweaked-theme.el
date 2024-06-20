;;; doom-horizon-theme.el --- Inspired by VSCode Horizon -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;;; This theme was inspired by the port of Horizon to Emacs
;;; see: https://github.com/aodhneine/horizon-theme.el

(require 'doom-themes)

;;; Code:
(defgroup doom-horizon-tweaked-theme nil
  "Options for the `doom-horizon' theme."
  :group 'doom-themes)

(defcustom doom-horizon-tweaked-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-horizon-tweaked-theme
  :type 'boolean)

(defcustom doom-horizon-tweaked-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-horizon-tweaked-theme
  :type 'boolean)

(defcustom doom-horizon-tweaked-comment-bg doom-horizon-tweaked-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their legibility."
  :group 'doom-horizon-tweaked-theme
  :type 'boolean)

(defcustom doom-horizon-tweaked-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to determine the exact padding."
  :group 'doom-horizon-tweaked-theme
  :type '(choice integer boolean))

;;
(def-doom-theme doom-horizon-tweaked
  "A port of the port of the Visual Studio Code theme Horizon"

  ;; name       default    256       16
  ((bg         '("#232530" nil       nil            ))
   (bg-alt     '("#1c1e26" nil       nil            ))
   (base0      '("#16161c" "black"   "black"        ))
   (base1      '("#1a1c23" "#111111" "brightblack"  ))
   (base2      '("#1d1f27" "#333333" "brightblack"  ))
   (base3      '("#232530" "#555555" "white"        ))
   (base4      '("#6a6a6a" "#6a6a6a" "white"        ))
   (base5      '("#f9cec3" "#f9cec3" "white"        ))
   (base6      '("#f9cbbe" "#f9cbbe" "white"        ))
   (base7      '("#fadad1" "#fadad1" "white"        ))
   (base8      '("#fdf0ed" "#fdf0ed" "white"        ))
   (fg-alt     '("#fdf0ed" "#fdf0ed" "brightwhite"  ))
   (fg         '("#B6AEBD" "#c7c9cb" "white"        ))

   (grey       base4)
   (red        '("#e95678" "#e95678" "red"          ))
   (orange     '("#f09383" "#f09383" "brightred"    ))
   (green      '("#09f7a0" "#09f7a0" "green"        ))
   (teal       '("#87ceeb" "#87ceeb" "brightgreen"  ))
   (yellow     '("#fab795" "#fab795" "yellow"       ))
   (blue       '("#21bfc2" "#21bfc2" "brightblue"   ))
   (dark-blue  '("#25b2bc" "#25A5BC" "blue"         ))
   (magenta    '("#C485C4" "#6c6f93" "magenta"      ))
   (violet     '("#9D77DB" "#9D77DB" "brightmagenta"))
   (cyan       '("#59e3e3" "#59e3e3" "brightcyan"   ))
   (dark-cyan  '("#27d797" "#27d797" "cyan"   ))


   ;; additional highlighting colours for horizon
   (hor-highlight  `(,(doom-lighten (car base3) 0.1) ,@(cdr base2)))
   (hor-highlight-selected (doom-lighten base3 0.1))
   (hor-highlight-bright (doom-lighten base3 0.2))
   (hor-highlight-brighter (doom-lighten base3 0.5))

   ;; face categories -- required for all themes
   (highlight      red)
   (vertical-bar   base0)
   (selection      violet)
   (builtin        violet)
   (comments       (if doom-horizon-tweaked-brighter-comments magenta hor-highlight-bright))
   (doc-comments   (if doom-horizon-tweaked-brighter-comments magenta hor-highlight-bright))
   (constants      orange)
   (functions      teal)
   (keywords       violet)
   (methods        magenta)
   (operators      teal)
   (type           teal)
   (strings        yellow)
   (variables      red)
   (numbers        orange)
   (region         hor-highlight)
   (error          red)
   (warning        dark-cyan)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)


   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-bright doom-horizon-tweaked-brighter-modeline)
   (-modeline-pad
    (when doom-horizon-tweaked-padded-modeline
      (if (integerp doom-horizon-tweaked-padded-modeline) doom-horizon-tweaked-padded-modeline 4)))

   (modeline-fg     `(,(doom-darken (car fg) 0.2) ,@(cdr fg-alt)))
   (modeline-fg-alt `(,(doom-lighten (car bg) 0.2) ,@(cdr base3)))

   (modeline-bg (doom-darken violet 0.6))
   (modeline-bg-inactive base2))


  ;;;; Base theme face overrides
  (((font-lock-comment-face &override)
    :slant 'italic
    :background (if doom-horizon-tweaked-comment-bg (doom-lighten bg 0.03)))
   (fringe :background bg)
   (auto-dim-other-buffers-face :background (doom-darken bg 0.1))
   (auto-dim-other-buffers-hide-face :background (doom-darken bg 0.1))
   (link :foreground yellow :inherit 'underline)
   ((line-number &override) :foreground hor-highlight-selected)
   ((line-number-current-line &override) :foreground hor-highlight-brighter)
   (tooltip :background base0 :foreground fg)

;;;; fringe
   (cider-fringe-good-face :foreground (doom-darken dark-blue 0.3))

;;;; hide-show
   (+fold-hideshow-folded-face :foreground magenta :weight 'bold :background bg)

;;;; ace window
   (aw-leading-char-face :foreground red :weight 'bold :height 4.0)
   (aw-background-face :foreground grey)


;;;; dired
   (diredfl-dir-name :foreground violet)
   (diredfl-dir-heading :foreground teal :weight 'ultrabold :box `(:line-width 8 :color ,bg-alt))

   (lsp-details-face :foreground hor-highlight-bright)

   (lsp-headerline-breadcrumb-path-error-face :underline nil)
   (lsp-headerline-breadcrumb-path-warning-face :underline nil)
;;;; symex
   (symex--current-node-face  :background (doom-lighten bg 0.05))


;;;; company
   (company-box-background    :background base0 :foreground fg)
   (company-tooltip-common    :foreground red :weight 'bold)
   (company-tooltip-selection :background hor-highlight :foreground fg)

;;;; modeline

   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box `(:line-width 5 :color ,modeline-bg))
   (mode-line-inactive
    :background (doom-lighten base3 0.03) :foreground modeline-fg-alt
    :box `(:line-width 5 :color ,(doom-lighten base3 0.03)))
   (mode-line-emphasis :foreground (if -modeline-bright base8 highlight))
   (mode-line-highlight :background base1 :foreground fg)

   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))
   (doom-modeline-highlight :foreground (doom-lighten bg 0.3))
   (doom-modeline-project-dir :foreground red :inherit 'bold )
   (doom-modeline-buffer-path :foreground red)
   (doom-modeline-buffer-file :foreground fg)
   (doom-modeline-buffer-modified :foreground yellow :weight 'ultra-bold)
   (doom-modeline-panel :background base1)
   (doom-modeline-urgent :foreground modeline-fg)
   (doom-modeline-info :foreground cyan)


;;;; elscreen
   (elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")


;;;; evil
   (evil-ex-search          :background hor-highlight-selected :foreground fg)
   (evil-ex-lazy-highlight  :background hor-highlight :foreground fg)


;;;; ivy
   (ivy-current-match       :background hor-highlight :distant-foreground nil)
   (ivy-posframe-cursor     :background red :foreground base0)
   (ivy-minibuffer-match-face-2 :foreground red :weight 'bold)


;;;; orderless
   (orderless-match-face-1 :weight 'bold :foreground (doom-blend red fg 0.6) :background (doom-blend red bg 0.1))


;;;; mic-paren
   (paren-face-match    :background (doom-lighten bg 0.1) :weight 'ultra-bold :foreground red)
   (paren-face-mismatch :foreground yellow :background base0   :weight 'ultra-bold)
   (paren-face-no-match :inherit 'paren-face-mismatch :weight 'ultra-bold)


;;;; magit
   (magit-section-heading :foreground violet)
   (magit-branch-remote   :foreground orange)
   (magit-diff-removed  :foreground (doom-blend red magenta 0.6) :background (doom-blend magenta bg 0.05))
   (magit-diff-added  :foreground (doom-blend green blue 0.4) :background (doom-blend blue bg 0.05))
   (diff-refine-removed  :foreground (doom-blend red magenta 0.6) :weight 'semibold :background (doom-blend magenta bg 0.12))
   (diff-refine-added  :foreground (doom-blend green blue 0.4) :weight 'semibold :background (doom-blend blue bg 0.12))
   (magit-diff-removed-highlight :inherit 'magit-diff-removed)
   (magit-diff-added-highlight :inherit 'magit-diff-added)


;;;; outline <built-in>
   ((outline-1 &override) :foreground blue :background nil)


;;;; flycheck
   (flycheck-fringe-error :foreground (doom-blend red bg 0.8))
   (flycheck-fringe-warning :foreground (doom-blend dark-blue bg 0.8))
   (flycheck-warning :underline (list :color (doom-blend fg bg 0.3)))

;;;; solaire-mode
   (solaire-mode-line-face
    :inherit 'mode-line
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))


;;;; treemacs
   (treemacs-root-face :foreground fg :weight 'bold :height 1.2)
   (doom-themes-treemacs-root-face :foreground fg :weight 'ultra-bold :height 1.2)
   (doom-themes-treemacs-file-face :foreground fg)
   (treemacs-directory-face :foreground fg)
   (treemacs-git-modified-face :foreground green)



;;;; rainbow parens
   (rainbow-delimiters-depth-1-face :foreground (doom-lighten base3 0.3))
   (rainbow-delimiters-depth-2-face :foreground (doom-lighten base3 0.3))
   (rainbow-delimiters-depth-3-face :foreground (doom-lighten base3 0.3))
   (rainbow-delimiters-depth-4-face :foreground (doom-lighten base3 0.3))
   (rainbow-delimiters-depth-5-face :foreground (doom-lighten base3 0.3))
   (rainbow-delimiters-depth-6-face :foreground (doom-lighten base3 0.3))
   (rainbow-delimiters-depth-7-face :foreground (doom-lighten base3 0.3))

;;;; org <built-in> <modes:org-mode>
   (org-archived                 :foreground doc-comments)
   (org-block                    :background base3    :extend t)
   (org-block-background         :background base3    :extend t)
   (org-block-begin-line         :inherit 'org-block  :foreground comments)
   (org-block-end-line           :inherit 'org-block-begin-line)
   (org-checkbox                 :inherit 'org-todo)
   (org-checkbox-statistics-done :inherit 'org-done)
   (org-checkbox-statistics-todo :inherit 'org-todo)
   (org-cite                     :foreground (doom-blend teal fg 0.9))
   (org-cite-key                 :foreground (doom-blend teal fg 0.6) :underline t)
   (org-code                     :inherit 'org-block :foreground orange)
   (org-date                     :foreground yellow)
   (org-default                  :inherit 'variable-pitch)
   (org-document-info            :foreground builtin)
   (org-document-title           :foreground builtin         :weight 'bold)
   (org-done                     :inherit 'org-headline-done :strike-through nil :weight 'bold)
   (org-drawer                   :foreground comments)
   (org-ellipsis                 :underline nil              :background nil :foreground comments)
   (org-footnote                 :foreground orange)
   (org-formula                  :foreground cyan)
   (org-headline-done            :foreground base5)
   (org-hide                     :foreground hidden)
   (org-latex-and-related        :foreground base8           :weight 'bold)
   (org-link                     :foreground (doom-blend base7 bg 0.6) :underline t :weight 'bold)
   (org-list-dt                  :foreground grey :weight 'bold)
   (org-meta-line                :foreground comments)
   (org-priority                 :foreground red)
   (org-property-value           :foreground doc-comments)
   (org-quote                    :inherit 'org-block :slant 'italic)
   (org-special-keyword          :foreground doc-comments    :underline nil)
   (org-table                    :foreground (doom-blend teal bg 0.7))
   (org-tag                      :foreground doc-comments    :weight 'normal)
   (org-todo                     :foreground dark-cyan           :bold 'inherit)
   (org-verbatim                 :foreground (doom-blend fg violet 0.5))
   (org-warning                  :foreground warning)
   ;; Omitted because we rely on style they inherit from the outline-N faces
   (org-level-1 :inherit 'outline-1 :height 1.4)
   (org-level-2 :inherit 'outline-2 :height 1.2)
   (org-level-3 :inherit 'outline-3 :height 1.1)
   ;;(org-level-4)
   ;;(org-level-5)
   ;;(org-level-6)
   ;;(org-level-7)
   ;;(org-level-8)
    ;;;; org-agenda <built-in>
   (org-agenda-done :inherit 'org-done)
   (org-agenda-dimmed-todo-face :foreground comments)
   (org-agenda-date          :foreground violet :weight 'ultra-bold)
   (org-agenda-date-today    :foreground (doom-lighten violet 0.4)   :weight 'ultra-bold)
   (org-agenda-date-weekend  :foreground (doom-darken violet 0.4)  :weight 'ultra-bold)
   (org-agenda-structure     :foreground fg :weight 'ultra-bold)
   (org-agenda-clocking      :background (doom-blend blue bg 0.2))
   (org-upcoming-deadline         :foreground (doom-blend fg bg 0.8))
   (org-upcoming-distant-deadline :foreground (doom-blend fg bg 0.5))
   (org-scheduled            :foreground fg)
   (org-scheduled-today      :foreground base7)
   (org-scheduled-previously :foreground base8)
   (org-time-grid            :foreground comments)
   (org-sexp-date            :foreground fg)

   ;; org-habit
   (org-habit-clear-face          :weight 'bold :background base4)
   (org-habit-clear-future-face   :weight 'bold :background base3)
   (org-habit-ready-face          :weight 'bold :background (doom-blend blue bg-alt 0.5))
   (org-habit-ready-future-face   :weight 'bold :background (doom-blend blue bg-alt 0.3))
   (org-habit-alert-face          :weight 'bold :background (doom-blend yellow bg-alt 0.5))
   (org-habit-alert-future-face   :weight 'bold :background (doom-blend yellow bg-alt 0.3))
   (org-habit-overdue-face        :weight 'bold :background (doom-blend red bg-alt 0.5))
   (org-habit-overdue-future-face :weight 'bold :background (doom-blend red bg-alt 0.3))

   ;; org-journal <modes:org-journal-mode>
   (org-journal-highlight :foreground highlight)
   (org-journal-calendar-entry-face :foreground magenta :slant 'italic)
   (org-journal-calendar-scheduled-face :foreground red :slant 'italic)

   ;; org-pomodoro
   (org-pomodoro-mode-line :foreground red)
   (org-pomodoro-mode-line-overtime :foreground warning :weight 'bold)

   ;; org-ref
   (org-ref-acronym-face    :foreground violet)
   (org-ref-cite-face       :foreground yellow :weight 'light :underline t)
   (org-ref-glossary-face   :foreground magenta)
   (org-ref-label-face      :foreground blue)
   (org-ref-ref-face        :inherit 'link :foreground teal)

   ((org-block &override) :background base1)
   ((org-block-begin-line &override) :background base1 :foreground comments)
   (org-agenda-done :foreground cyan)





;;;; haskell-mode
   (haskell-type-face :foreground violet)
   (haskell-constructor-face :foreground yellow)
   (haskell-operator-face :foreground fg)
   (haskell-literate-comment-face :foreground hor-highlight-selected)

;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground violet)
   (css-property             :foreground fg)
   (css-selector             :foreground red)

;;;; js2-mode
   (js2-object-property        :foreground red)

;;;; markdown-mode
   (markdown-markup-face           :foreground cyan)
   (markdown-link-face             :foreground orange)
   (markdown-link-title-face       :foreground yellow)
   (markdown-header-face           :foreground red :inherit 'bold)
   (markdown-header-delimiter-face :foreground red :inherit 'bold)
   (markdown-language-keyword-face :foreground orange)
   (markdown-markup-face           :foreground fg)
   (markdown-bold-face             :foreground violet)
   (markdown-table-face            :foreground fg :background base1)
   ((markdown-code-face &override) :foreground orange :background base1)

;;;; rjsx-mode
   (rjsx-tag :foreground red)
   (rjsx-tag-bracket-face :foreground red)
   (rjsx-attr :foreground cyan :slant 'italic :weight 'medium)
   (tide-hl-identifier-face :background hor-highlight)

;;;; web-mode
   (web-mode-html-tag-bracket-face :foreground red)
   (web-mode-html-tag-face         :foreground red)
   (web-mode-html-attr-name-face   :foreground orange)





   ))

;;; doom-horizon-tweaked-theme.el ends here
