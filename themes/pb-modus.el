;;; pb-modus.el --- My modus based theme -*- lexical-binding: t; -*-

;;; Commentary:
;;  My modus light theme
;;
;;; Code:

(require 'km)
(require 'modus-themes)
(require 'modus-operandi-theme)
(require 'pb-color)
(require 'pb-macros)

(defun pb-modus-warmer (c delta)
  "Warm C by DELTA."
  (if (< .166
         (pc/hue c)
         .666)
      (pc/rotate c (- delta))
    (pc/rotate c delta)))

(defun pb-modus-cooler (c delta)
  "Cool C by DELTA."
  (if (< .166
         (pc/hue c)
         .666)
      (pc/rotate c delta)
    (pc/rotate c (- delta))))

(defun pb-modus-color-variations (c)
  "Build a list of variations for the given color C."
  (let (combinations)
    (dolist (h (list
                nil
                (cons :warmer (fn (c) (pb-modus-warmer c 0.05)))
                (cons :cooler (fn (c) (pb-modus-cooler c 0.05)))))
      (dolist (s (list
                  nil
                  (cons :intense (fn (c) (pb-color_saturate c 0.5)))
                  (cons :faint (fn (c) (pb-color_desaturate c 0.3)))))
        (dolist (l (list
                    nil
                    (cons :lighter (fn (c) (pb-color_lighten c 0.1)))
                    (cons :darker (fn (c) (pb-color_darken c 0.1)))))
          (let ((xs (remove nil (list h s l))))
            (if xs
                (push (cons (pb_join-keyword (mapcar #'car xs) "-")
                            (seq-reduce (fn (ret f)
                                            (funcall f ret))
                                        (mapcar #'cdr xs)
                                        c))
                      combinations))))))
    (nreverse combinations)))

(defun pb-modus-build-colors (&optional base)
  "Build a rich color palette based on BASE (hex color string)."
  (let (combinations)
    (dolist (c (seq-mapn #'cons
                         pc/12-hue-names
                         (pc/hue-wheel (or base (pc/from-hsl (list 0 .5 .5)))
                                       12)))
      (push (list (pb_symbol (car c))
                  (cdr c))
            combinations)
      (dolist (variation (pb-modus-color-variations (cdr c)))
        (push (list (pb_symbol (car c) (car variation))
                    (cdr variation))
              combinations)))
    (nreverse combinations)))

(defvar pb-modus-colors
  (append `((fg-intense ,(pc/from-hsl (list 0 0 .35)))
            (fg-main ,(pc/from-hsl (list 0 0 .45)))
            (fg-dim ,(pc/from-hsl (list 0 0 .55)))
            (fg-faint ,(pc/from-hsl (list 0 0 .65)))
            (bg-intense ,(pc/from-hsl (list 0 0 1)))
            (bg-main ,(pc/from-hsl (list 0 0 .97)))
            (bg-dim ,(pc/from-hsl (list 0 0 .94)))
            (bg-faint ,(pc/from-hsl (list 0 0 .8))))
          (pb-modus-build-colors)))

(defun pb-modus-get-color (name)
  "Retrieve a color by NAME from `pb-modus-colors'."
  (car-safe (alist-get name pb-modus-colors)))

(setq modus-operandi-palette-user
      pb-modus-colors)

(setq modus-operandi-palette-overrides
      `(;; various fg/bg

        (bg-hl-line bg-dim)

        (bg-region bg-dim)
        (fg-region unspecified)

        (bg-paren-match bg-dim)
        (fg-paren-match fg-main)

        ;; Modeline
        (bg-mode-line-active bg-faint)
        (fg-mode-line-active fg-intense)
        (border-mode-line-active bg-faint)
        (bg-mode-line-inactive bg-dim)
        (fg-mode-line-inactive fg-dim)
        (border-mode-line-inactive bg-dim)
        (modeline-err red)
        (modeline-warning yellow)
        (modeline-info azure)

        ;; Headings
        (fg-heading-0 cyan-faint-lighter)
        (fg-heading-1 rose-faint-lighter)
        (fg-heading-2 violet-faint-lighter)
        (fg-heading-3 cyan-faint-lighter)
        (fg-heading-4 rose-faint-lighter)
        (fg-heading-5 violet-faint-lighter)
        (fg-heading-6 cyan-faint-lighter)
        (fg-heading-7 rose-faint-lighter)
        (fg-heading-8 violet-faint-lighter)

        ;; Code
        (builtin red-warmer-intense-lighter)
        (comment orange-lighter)
        (constant cyan-faint-darker)
        (docstring orange-lighter)
        (docmarkup cyan-faint)
        (fnname magenta-faint-lighter)
        (keyword azure)
        (preprocessor red-cooler)
        (string orange-warmer-intense-darker)
        (type cyan-cooler)
        (variable spring-darker)
        (rx-construct green-cooler)
        (rx-backslash magenta)

        (rainbow-0 "gray60")
        (rainbow-1 "gray60")
        (rainbow-2 "gray60")
        (rainbow-3 "gray60")
        (rainbow-4 "gray60")
        (rainbow-5 "gray60")
        (rainbow-6 "gray60")
        (rainbow-7 "gray60")
        (rainbow-8 "gray60")))

(progn


  (require 'symex)
  (require 'ibuffer)

  (defun pb-modus-theme-hook ()

    (setq evil-normal-state-cursor `(box ,(pb-modus-get-color 'red-lighter))
          evil-insert-state-cursor `((bar . 3) ,(pb-modus-get-color 'cyan-intense-darker))
          evil-visual-state-cursor `(box ,(pb-modus-get-color 'yellow-warmer-intense-lighter))
          evil-symex-state-cursor (list 'box (pb-modus-get-color 'cyan)))

    (set-face-attribute 'symex--current-node-face nil
                        :background (pb-color_blend (pb-modus-get-color 'bg-main)
                                                    (pb-modus-get-color 'cyan-faint-lighter)
                                                    .9))

    (set-face-attribute 'default nil :foreground (pb-modus-get-color 'fg-main))

    (set-face-attribute 'nerd-icons-ibuffer-file-face nil
                        :foreground (pb-modus-get-color 'fg-dim))

    (set-face-attribute 'font-lock-delimiter-face nil
                        :foreground (pb-color (pb-modus-get-color 'bg-main)
                                              (lighten .3)))

    (set-face-attribute 'elisp-shorthand-font-lock-face nil
                        :weight 'bold
                        :foreground (pb-modus-get-color 'magenta-faint-lighter))

    (set-face-attribute 'doom-modeline-bar nil
                        :background (pb-modus-get-color 'azure))

    (set-face-attribute 'cider-result-overlay-face nil
                        :box (list :line-width 2 :color (pb-modus-get-color 'bg-intense))
                        :background (pb-modus-get-color 'bg-intense))

    (set-face-attribute 'org-block-begin-line nil
                        :foreground (pb-modus-get-color 'fg-faint))

    (setq ibuffer-filter-group-name-face
          (list :foreground (pb-modus-get-color 'orange-lighter)
                :box (list :line-width 10 :color (pb-modus-get-color 'bg-main))
                :weight 'bold
                :height 1.1))

    (setq ibuffer-title-face
          (list :foreground (pc/blend (pb-modus-get-color 'fg-main) (pb-modus-get-color 'bg-main) 0.3)
                :weight 'normal
                :height 1.1)))

  (add-hook 'modus-themes-post-load-hook
            #'pb-modus-theme-hook))

(modus-themes-select 'modus-operandi)

;; Local Variables:
;; read-symbol-shorthands: (("pc/" . "pb-color_"))
;; End:

;;; pb-modus.el ends here
