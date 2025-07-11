;;; pb/utils/pb-style.el -*- lexical-binding: t; -*-

(require 'flycheck)
(require 'pb)
(require 'pb-color)
(require 'doom-themes)

(defun pb-style/set-local-fringe-face (color)
  (face-remap-add-relative 'fringe :background color)
  (flycheck-refresh-fringes-and-margins))

(defun pb-style/reset-local-fringe-face ()
  (face-remap-add-relative 'fringe :background (face-attribute 'default :background))
  (flycheck-refresh-fringes-and-margins))

(pb/comment (pb-style/reset-local-fringe-face)
            (pb-style/set-local-fringe-face (pb-color (doom-color 'green)
                                                      (desaturate 0.5)
                                                      (darken 0.3))))

(progn :generated
       (defun pb-style/highlight-current-buffer ()
         "Add a subtle background color to the current buffer to distinguish it."
         (interactive)
         (let ((color (pb-color (doom-color 'blue)
                                (desaturate 0.8)
                                (darken 0.4))))
           (face-remap-add-relative 'default :background color)))

       (defun pb-style/pulse-region (start end &optional face)
         "Temporarily highlight the region between START and END with FACE."
         (interactive "r")
         (let ((pulse-face (or face 'pulse-highlight-face)))
           (pulse-momentary-highlight-region start end pulse-face)))

       (defun pb-style/flash-mode-line ()
         "Flash the mode line to get attention."
         (interactive)
         (let ((original-bg (face-attribute 'mode-line :background))
               (flash-bg (doom-color 'yellow)))
           (face-remap-add-relative 'mode-line :background flash-bg)
           (run-with-timer 0.5 nil
                           (lambda ()
                             (face-remap-add-relative 'mode-line :background original-bg)))))

       ;; Line highlighting
       (defun pb-style/highlight-current-line-globally (&optional color)
         "Highlight current line in all windows with COLOR."
         (interactive)
         (global-hl-line-mode 1)
         (set-face-background 'hl-line
                              (or color
                                  (pb-color (doom-color 'blue)
                                            (desaturate 0.7)
                                            (darken 0.2)))))

       (defun pb-style/highlight-matching-delimiters ()
         "Make matching delimiters more visible."
         (interactive)
         (show-paren-mode 1)
         (setq show-paren-delay 0)
         (set-face-attribute 'show-paren-match nil
                             :background (pb-color (doom-color 'magenta)
                                                   (desaturate 0.4)
                                                   (darken 0.1))
                             :foreground (doom-color 'fg)
                             :weight 'bold))

       ;; Theme and mode-specific styling
       (defun pb-style/dim-non-focused-windows ()
         "Dim all non-focused windows."
         (interactive)
         (require 'dimmer)
         (dimmer-mode t)
         (setq dimmer-fraction 0.3)
         (dimmer-configure-which-key)
         (dimmer-configure-posframe))

       (defun pb-style/apply-code-style (&optional mode color)
         "Apply a specific style for coding MODE with background COLOR."
         (interactive)
         (let* ((mode-name (or mode major-mode))
                (bg-color (or color
                              (cond
                               ((eq mode-name 'clojure-mode)
                                (pb-color (doom-color 'blue) (desaturate 0.8) (darken 0.3)))
                               ((eq mode-name 'emacs-lisp-mode)
                                (pb-color (doom-color 'teal) (desaturate 0.8) (darken 0.3)))
                               ((eq mode-name 'org-mode)
                                (pb-color (doom-color 'green) (desaturate 0.8) (darken 0.3)))
                               (t (pb-color (doom-color 'violet) (desaturate 0.8) (darken 0.3)))))))
           (pb-style/set-local-fringe-face bg-color)
           (face-remap-add-relative 'default :background
                                    (pb-color bg-color (lighten 0.05)))))
       (defun pb-style/cycle-font-size (increment)
         "Increase or decrease font size by INCREMENT."
         (interactive "nSize increment (can be negative): ")
         (let ((new-height (+ (face-attribute 'default :height) increment)))
           (set-face-attribute 'default nil :height new-height)
           (message "Font size: %s" new-height)))

       (defun pb-style/toggle-line-spacing ()
         "Toggle between normal and increased line spacing."
         (interactive)
         (if (eq line-spacing nil)
             (setq line-spacing 0.5)
           (setq line-spacing nil))
         (message "Line spacing set to: %s" (or line-spacing "default")))

       (defun pb-style/toggle-emphasis-region (start end)
         "Toggle extra emphasis on region from START to END."
         (interactive "r")
         (if (get-text-property start 'pb-style-emphasized)
             (remove-text-properties start end '(face nil pb-style-emphasized nil))
           (let ((emphasis-face `(:inherit default
                                  :foreground ,(doom-color 'blue)
                                  :weight bold
                                  :height 1.1)))
             (put-text-property start end 'face emphasis-face)
             (put-text-property start end 'pb-style-emphasized t)))))

(provide 'pb-style)
