;;; pb/xp/ace-window-theme-tweaks.el -*- lexical-binding: t; -*-

(setq ibuffer-mode-hook ())

(defun pb-ibuffer-font-lock-setup ()
  "Set up font-lock for ibuffer."
  (font-lock-add-keywords
   nil
   '(("^ \\(\\\\*\\)  .*"
      (0 (prog1 ()
           (set-face-attribute 'font-lock-keyword-face nil
                               :foreground "red" :weight 'bold))))
     ("^  \\(\\%\\) .*"
      (0 (prog1 ()
           (set-face-attribute 'font-lock-keyword-face nil
                               :foreground "green" :weight 'bold)))))))
(add-hook 'ibuffer-mode-hook 'pb-ibuffer-font-lock-setup)

(defun pb-ibuffer-font-lock-setup ()
  "Set up font-lock for ibuffer."
  (font-lock-add-keywords
   nil
   '(("^ \\(\\*\\)   .*"
      (2 (set-face-attribute 'font-lock-keyword-face nil
                             :foreground "green" :weight 'bold))))))
(add-hook 'ibuffer-mode-hook 'pb-ibuffer-font-lock-setup)
