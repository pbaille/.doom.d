(deftheme custom1
  "first cutom theme")

(custom-theme-set-faces
 'custom1
 '(default ((t (:family "Fira Code" :foundry "nil" :width normal :height 160 :weight normal :slant normal :underline nil :overline nil :extend nil :strike-through nil :box nil :inverse-video nil :foreground "#B6AEBD" :background "#232530" :stipple nil :inherit nil))))
 '(cursor ((t (:background "#ff9940"))))
 '(fixed-pitch ((t (:family "Fira Code" :foundry "nil" :height 160))))
 '(variable-pitch ((((type w32)) (:foundry "outline" :family "Arial")) (t (:family "Sans Serif"))))
 '(escape-glyph ((t (:foreground "#59e3e3"))))
 '(homoglyph ((((background dark)) (:foreground "cyan")) (((type pc)) (:foreground "magenta")) (t (:foreground "brown"))))
 '(minibuffer-prompt ((t (:foreground "#e95678"))))
 '(highlight ((t (:foreground "#16161c" :background "#e95678"))))
 '(region ((t (:extend t :background "#383a44"))))
 '(shadow ((t (:foreground "#f9cec3"))))
 '(secondary-selection ((t (:extend t :background "#6a6a6a"))))
 '(trailing-whitespace ((t (:background "#e95678"))))
 '(font-lock-builtin-face ((t (:foreground "#9D77DB"))))
 '(font-lock-comment-delimiter-face ((default (:inherit (font-lock-comment-face)))))
 '(font-lock-comment-face ((t (:slant italic :foreground "#4e5059"))))
 '(font-lock-constant-face ((t (:foreground "#f09383"))))
 '(font-lock-doc-face ((t (:foreground "#4e5059" :inherit (font-lock-comment-face)))))
 '(font-lock-doc-markup-face ((t (:inherit (font-lock-constant-face)))))
 '(font-lock-function-name-face ((t (:foreground "#87ceeb"))))
 '(font-lock-keyword-face ((t (:foreground "#9D77DB"))))
 '(font-lock-negation-char-face ((t (:foreground "#87ceeb" :inherit (bold)))))
 '(font-lock-preprocessor-face ((t (:foreground "#87ceeb" :inherit (bold)))))
 '(font-lock-regexp-grouping-backslash ((t (:foreground "#87ceeb" :inherit (bold)))))
 '(font-lock-regexp-grouping-construct ((t (:foreground "#87ceeb" :inherit (bold)))))
 '(font-lock-string-face ((t (:foreground "#fab795"))))
 '(font-lock-type-face ((t (:foreground "#87ceeb"))))
 '(font-lock-variable-name-face ((t (:foreground "#e95678"))))
 '(font-lock-warning-face ((t (:inherit (warning)))))
 '(button ((t (:inherit (link)))))
 '(link ((t (:foreground "#fab795" :inherit (underline)))))
 '(link-visited ((default (:inherit (link))) (((class color) (background light)) (:foreground "magenta4")) (((class color) (background dark)) (:foreground "violet"))))
 '(fringe ((t (:background "#232530"))))
 '(header-line ((t (:inherit (mode-line)))))
 '(tooltip ((t (:foreground "#B6AEBD" :background "#16161c"))))
 '(mode-line ((t (:box nil :foreground "#918b97" :background "#1a1c23"))))
 '(mode-line-buffer-id ((t (:weight bold))))
 '(mode-line-emphasis ((t (:foreground "#e95678"))))
 '(mode-line-highlight ((t (:foreground "#B6AEBD" :background "#1a1c23"))))
 '(mode-line-inactive ((t (:box nil :foreground "#4e5059" :background "#1a1c23"))))
 '(isearch ((t (:weight bold :inherit (lazy-highlight)))))
 '(isearch-fail ((t (:weight bold :foreground "#16161c" :background "#e95678"))))
 '(lazy-highlight ((t (:weight bold :foreground "#fdf0ed" :background "#a33c54"))))
 '(match ((t (:weight bold :foreground "#09f7a0" :background "#16161c"))))
 '(next-error ((t (:inherit (region)))))
 '(query-replace ((t (:inherit (isearch))))))

(provide-theme 'custom1)
