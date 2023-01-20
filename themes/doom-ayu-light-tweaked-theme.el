;;; doom-ayu-light-tweaked-theme.el --- inspired by Ayu Mirage -*- lexical-binding: t; no-byte-compile: t; -*-

(require 'doom-themes)


;;
;; Variables

(defgroup doom-ayu-light-tweaked-theme
  nil
  "Options for the `doom-ayu-light-tweaked' theme."
  :group 'doom-themes)

(defcustom doom-ayu-light-tweaked-brighter-modeline
  nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-ayu-light-tweaked-theme
  :type 'boolean)

(defcustom doom-ayu-light-tweaked-brighter-comments
  nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-ayu-light-tweaked-theme
  :type 'boolean)

(defcustom doom-ayu-light-tweaked-comment-bg
  doom-ayu-light-tweaked-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-ayu-light-tweaked-theme
  :type 'boolean)

(defcustom doom-ayu-light-tweaked-padded-modeline
  doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-ayu-light-tweaked-theme
  :type '(choice integer boolean))

;;
(def-doom-theme doom-ayu-light-tweaked
  "A light theme inspired by Ayu Light"

  ;; name        default   256       16
  (
   ;; common
   (common-accent   '("#E27872" "orange"  "orange" ))
   (common-bg       '("#373737" "black"   "black"  ))
   (common-fg       '("#89919A" "grey"    "grey"   ))
   (common-ui       '("#ba9199" "grey"    "grey"   ))
   (test            '("#2ea8e6" "grey"    "grey"   ))

   ;; syntax
   (syntax-tag      '("#55b4d4" "cyan"    "blue"   ))
   (syntax-func     '("#F18170" "yellow"  "yellow" ))
   (syntax-entity   '("#9B6DC1" "blue"    "blue"   ))
   (syntax-string   '("#8FBF00" "green"   "green"  ))
   (syntax-regexp   '("#4cbf99" "teal"    "green"  ))
   (syntax-markup   '("#f07171" "red"     "red"    ))
   (syntax-keyword  '("#63ACD3" "orange"  "orange" ))
   (syntax-special  '("#EEA55B" "yellow"  "yellow" ))
   (syntax-comment  '("#abb0b6" "grey"    "grey"   ))
   (syntax-constant '("#a37acc" "magenta" "purple" ))
   (syntax-operator '("#ed9366" "orange"  "orange" ))
   (syntax-error    '("#f51818" "red"     "red"    ))

   ;; ui
   (ui-line               (doom-darken common-bg 0.5))
   (ui-panel-shadow       (doom-lighten common-bg 0.5))
   (ui-panel-border       (doom-lighten common-bg 0.1))
   (ui-gutter-normal      (doom-lighten common-ui 0.0))
   (ui-gutter-active      common-ui)
   (ui-selection-bg       (doom-blend common-bg test 0.7))
   (ui-selection-inactive (doom-darken common-bg 0.05))
   (ui-selection-border   (doom-darken common-bg 0.05))
   (ui-guide-active       (doom-lighten common-ui 0.75))
   (ui-guide-normal       (doom-lighten common-ui 0.35))
   (ui-org-block          (doom-darken common-bg 0.05))
   (elscreen-bg           (doom-lighten common-fg 0.35))
   (elscreen-fg           (doom-darken common-fg 0.85))

   ;; vcs
   (vcs-added    '("#99bf4d" "green" "green" ))
   (vcs-modified '("#709ecc" "blue"  "blue"  ))
   (vcs-removed  '("#f27983" "red"   "red"   ))

   (bg         common-bg)
   (bg-alt     common-bg)
   (base0      ui-gutter-normal)
   (base1      ui-gutter-active)
   (base2      ui-selection-bg)
   (base3      ui-selection-border)
   (base4      ui-selection-inactive)
   (base5      ui-guide-active)
   (base6      ui-guide-normal)
   (base7      ui-panel-shadow)
   (base8      ui-panel-border)
   (fg         common-fg)
   (fg-alt     common-ui)

   (grey       syntax-comment)
   (red        syntax-markup)
   (orange     syntax-keyword)
   (green      syntax-string)
   (teal       syntax-regexp)
   (yellow     syntax-func)
   (blue       syntax-entity)
   (dark-blue  (doom-darken syntax-entity 0.2))
   (magenta    syntax-constant)
   (violet     (doom-lighten syntax-constant 0.2))
   (cyan       syntax-tag)
   (dark-cyan  (doom-darken syntax-tag 0.2))

   ;; face categories
   (highlight      common-accent)
   (vertical-bar   ui-panel-border)
   (selection      ui-selection-inactive)
   (builtin        syntax-constant)
   (comments (doom-blend common-bg common-fg 0.5))
   (doc-comments   (doom-lighten syntax-comment 0.1))
   (constants      syntax-constant)
   (functions      syntax-func)
   (keywords       syntax-keyword)
   (methods        syntax-func)
   (operators      syntax-operator)
   (type           syntax-special)
   (strings        syntax-string)
   (variables      (doom-blend fg syntax-func 0.6))
   (numbers        syntax-func)
   (region         ui-selection-bg)
   (error          syntax-error)
   (warning        yellow)
   (success        green)
   (vc-modified    vcs-modified)
   (vc-added       vcs-added)
   (vc-deleted     vcs-removed)

   ;; custom categories
   (hidden     (car bg))
   (-modeline-bright doom-ayu-light-tweaked-brighter-modeline)
   (-modeline-pad
    (when doom-ayu-light-tweaked-padded-modeline
      (if (integerp doom-ayu-light-tweaked-padded-modeline) doom-ayu-light-tweaked-padded-modeline 4)))

   (modeline-fg     common-ui)
   (modeline-fg-alt base5)

   (modeline-bg (doom-darken bg 0.3))
   (modeline-bg-l (doom-darken violet 0.3))
   (modeline-bg-inactive (doom-lighten bg 0.1))
   (modeline-bg-inactive-l bg))

  ;;;; Base theme face overrides
  (((line-number &override) :foreground (doom-blend bg fg 0.5))
   ((line-number-current-line &override) :foreground fg)
   ((font-lock-comment-face &override)
    :background (if doom-ayu-light-tweaked-comment-bg (doom-lighten bg 0.05)))
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground (if -modeline-bright base8 highlight))

   ;; rainbow delimiters
   (rainbow-delimiters-depth-1-face :foreground (doom-blend common-bg blue 0.6))
   (rainbow-delimiters-depth-2-face :foreground (doom-blend common-bg magenta 0.6))
   (rainbow-delimiters-depth-3-face :foreground (doom-blend common-bg green 0.6))
   (rainbow-delimiters-depth-4-face :foreground (doom-blend common-bg violet 0.6))
   (rainbow-delimiters-depth-5-face :foreground (doom-blend common-bg teal 0.6))
   (rainbow-delimiters-depth-6-face :foreground (doom-blend common-bg blue 0.6))
   (rainbow-delimiters-depth-7-face :foreground (doom-blend common-bg magenta 0.6))
   (rainbow-delimiters-depth-8-face :foreground (doom-blend common-bg green 0.6))
   (rainbow-delimiters-depth-9-face :foreground (doom-blend common-bg violet 0.6))
   (rainbow-delimiters-base-error-face :inherit 'rainbow-delimiters-base-face :foreground error)
   (rainbow-delimiters-base-face :inherit 'default)
   (rainbow-delimiters-unmatched-face  :foreground red :weight 'bold :inverse-video t)
   (rainbow-delimiters-mismatched-face :inherit 'rainbow-delimiters-unmatched-face)

   ;; paren match
   (paren-face-match    :underline red :foreground red   :background (doom-lighten bg 0.2) :weight 'ultra-bold)
   (paren-face-mismatch :foreground base0 :background red   :weight 'ultra-bold)
   (paren-face-no-match :inherit 'paren-face-mismatch :weight 'ultra-bold)

   ;;;; company
   (company-tooltip :foreground common-fg :background common-bg)
   (company-tooltip-annotation :foreground common-fg)
   (company-tooltip-selection :background ui-line)
   (company-tooltip-search :foreground common-accent :weight 'bold)
   (company-scrollbar-bg :background common-bg)
   (company-scrollbar-fg :background syntax-comment)

   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)

   ;;;; diff-mode <built-in>
   (diff-removed :foreground vcs-removed)

   ;;;; doom-modeline
   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight) :weight 'normal)
   (doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'normal)
   (doom-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'normal)
   (doom-modeline-buffer-project-root :foreground green :weight 'normal)

   ;;;; elscreen
   (elscreen-tab-other-screen-face :background elscreen-bg :foreground elscreen-fg)

   ;;;; highlight-numbers
   (highlight-numbers-number :foreground syntax-func :weight 'normal)

   ;;;; ivy
   (ivy-current-match :background ui-line)
   (ivy-minibuffer-match-face-1 :foreground common-accent :weight 'bold)
   (ivy-minibuffer-match-face-2 :foreground common-accent :weight 'bold)
   (ivy-minibuffer-match-face-3 :foreground common-accent :weight 'bold)
   (ivy-minibuffer-match-face-4 :foreground common-accent :weight 'bold)

   ;;;; js2-mode
   (js2-object-property :foreground common-fg)

   ;;;; LaTeX-mode
   (font-latex-math-face :foreground green)

   ;;;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   ((markdown-code-face &override) :background (doom-lighten common-bg 0.05))

   ;;;; org <built-in>
   (org-hide :foreground hidden)
   (org-headline-done :foreground syntax-comment)
   (org-document-info-keyword :foreground comments)

   ;;;; rjsx-mode
   (rjsx-tag :foreground cyan)
   (rjsx-tag-bracket-face :foreground (doom-lighten cyan 0.5))
   (rjsx-attr :foreground syntax-func)

   ;;;; solaire-mode
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))

   ;;;; web-mode
   (web-mode-html-tag-face :foreground cyan)
   (web-mode-html-tag-bracket-face :foreground (doom-lighten cyan 0.5))
   (web-mode-html-attr-name-face :foreground syntax-func)))

;;; doom-ayu-light-tweaked-theme.el ends here
