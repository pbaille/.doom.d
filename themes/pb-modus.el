;;; pb-modus.el --- My modus based theme -*- lexical-binding: t; -*-

;;; Commentary:
;;  My modus light theme
;;
;;; Code:

(require 'km)
(require 'modus-themes)
(require 'modus-operandi-theme)
(require 'pb)
(require 'pb-color)

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
                (cons :warmer (lambda (c) (pb-modus-warmer c 0.05)))
                (cons :cooler (lambda (c) (pb-modus-cooler c 0.05)))))
      (dolist (s (list
                  nil
                  (cons :intense (lambda (c) (pb-color_saturate c 0.5)))
                  (cons :faint (lambda (c) (pb-color_desaturate c 0.3)))))
        (dolist (l (list
                    nil
                    (cons :lighter (lambda (c) (pb-color_lighten c 0.1)))
                    (cons :darker (lambda (c) (pb-color_darken c 0.1)))))
          (let ((xs (remove nil (list h s l))))
            (if xs
                (push (cons (pb_join-keyword (mapcar #'car xs) "-")
                            (seq-reduce (lambda (ret f)
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

(defun pb-modus-bindings* (base bindings)
  "Incrementally extend BASE palette with BINDINGS.
BASE and BINDINGS are a list of bindings (list of length 2 lists).
BINDINGS right hand sides can refer to BASE bindings or previous ones.
Like the `let*' macro, each binding in BINDINGS is available to the nexts.
Additionally it provide syntax sugar for color transformations.
If a binding contains more than two component,
the cdr is interpreted as a `pb-color' form:
\(sym color trans1 trans2 ...)
which is tranformed to:
\(sym (pb-color color trans1 trans2 ...))"
  (eval `(let* ((unspecified 'unspecified)
                ,@base
                ,@(mapcar (lambda (x) (list (car x)
                                       (if (cddr x)
                                           (cons 'pb-color (cdr x))
                                         (cadr x))))
                          bindings))
           (list ,@(cl-reduce (lambda (bindings binding-sym)
                                (if (alist-get binding-sym bindings)
                                    bindings
                                  (cons `(list ',binding-sym ,binding-sym) bindings)))
                              (reverse (mapcar #'car (append base bindings)))
                              :initial-value ())))))

(defvar pb-modus-colors
  (pb-modus-bindings*
   (pb-modus-build-colors)
   '((ground "#ffffff")
     (fg-intense ground (set-lightness .35))
     (fg-main ground (set-lightness .45))
     (fg-dim ground (set-lightness .55))
     (fg-faint ground (set-lightness .65))
     (bg-intense ground (set-lightness 1))
     (bg-main ground (set-lightness .97))
     (bg-dim ground (set-lightness .94))
     (bg-faint ground (set-lightness .8)))))

(setq modus-operandi-palette-user
      pb-modus-colors)

(defun pb-modus-get-color (name)
  "Retrieve a color by NAME from `pb-modus-colors'."
  (car-safe (alist-get name modus-operandi-palette-user)))

(setq modus-operandi-palette-overrides
      (pb-modus-bindings*
       modus-operandi-palette-user
       '(
         ;; specific bg/fg
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
         (fg-heading-2 fg-heading-1 (rotate -0.07))
         (fg-heading-3 fg-heading-2 (rotate -0.07))
         (fg-heading-4 fg-heading-3 (rotate -0.07))
         (fg-heading-5 fg-heading-4 (rotate -0.07))
         (fg-heading-6 fg-heading-5 (rotate -0.07))
         (fg-heading-7 fg-heading-6 (rotate -0.07))
         (fg-heading-8 fg-heading-7 (rotate -0.07))
         (fg-heading-9 fg-heading-8 (rotate -0.07))

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
         (rainbow-8 "gray60"))))

(progn


  (require 'symex)
  (require 'ibuffer)
  (require 'nerd-icons-ibuffer)

  (defun pb-modus-theme-hook ()

    (setq evil-normal-state-cursor `(box ,(pb-modus-get-color 'red-lighter))
          evil-insert-state-cursor `((bar . 3) ,(pb-modus-get-color 'cyan-intense-darker))
          evil-visual-state-cursor `(box ,(pb-modus-get-color 'yellow-warmer-intense-lighter))
          evil-symex-state-cursor (list 'box (pb-modus-get-color 'cyan)))

    (set-face-attribute 'symex--current-node-face nil
                        :inherit nil
                        :background (pb-color_blend (pb-modus-get-color 'bg-main)
                                                    (pb-modus-get-color 'cyan-faint-lighter)
                                                    .9))

    (set-face-attribute 'default nil :foreground (pb-modus-get-color 'fg-main))

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

    (set-face-attribute 'org-level-1 nil :inherit 'outline-1 :height 1.3 :box (list :line-width 8 :color (pb-modus-get-color 'bg-main)))
    (set-face-attribute 'org-level-2 nil :inherit 'outline-2 :height 1.15 :box (list :line-width 6 :color (pb-modus-get-color 'bg-main)))
    (set-face-attribute 'org-level-3 nil :inherit 'outline-3 :height 1.1 :box (list :line-width 5 :color (pb-modus-get-color 'bg-main)))
    (set-face-attribute 'org-level-4 nil :inherit 'outline-4 :box (list :line-width 4 :color (pb-modus-get-color 'bg-main)))
    (set-face-attribute 'org-level-5 nil :inherit 'outline-5 :box (list :line-width 3 :color (pb-modus-get-color 'bg-main)))
    (set-face-attribute 'org-level-6 nil :inherit 'outline-6 :box (list :line-width 2 :color (pb-modus-get-color 'bg-main)))
    (set-face-attribute 'org-level-7 nil :inherit 'outline-7 :box (list :line-width 1 :color (pb-modus-get-color 'bg-main)))

    (set-face-attribute 'nerd-icons-ibuffer-file-face
                        nil
                        :foreground (pb-modus-get-color 'fg-dim))

    (setq ibuffer-filter-group-name-face
          (list :foreground (pb-modus-get-color 'orange-lighter)
                :box (list :line-width 10 :color (pb-modus-get-color 'bg-main))
                :weight 'bold
                :height 1.1))

    (setq ibuffer-title-face
          (list :foreground (pc/blend (pb-modus-get-color 'fg-main) (pb-modus-get-color 'bg-main) 0.3)
                :weight 'normal
                :height 1.1))

    (set-face-attribute 'diredfl-dir-name nil
                        :foreground (pb-modus-get-color 'cyan-faint))

    (set-face-attribute 'diredfl-dir-heading nil
                        :box (list :line-width 10 :color (pb-modus-get-color 'bg-main))))

  (add-hook 'modus-themes-post-load-hook
            #'pb-modus-theme-hook))

(defun pb-modus-load ()
  "Load personal theme."
  (interactive)
  (modus-themes-select 'modus-operandi))

;; Local Variables:
;; read-symbol-shorthands: (("pc/" . "pb-color_"))
;; End:

;;; pb-modus.el ends here
