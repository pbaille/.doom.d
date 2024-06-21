;; -*- lexical-binding: t; -*-
(set-face-attribute 'minibuffer-prompt nil
                    :background "#ffffff")

(with-current-buffer " *Echo Area 0*"
  (face-remap-add-relative 'default '(:background "#ffffff" :family "Comic Sans")))

(with-current-buffer (get-buffer " *Echo Area 0*")                             ; the leading space character is correct
  (setq-local face-remapping-alist '((default (:background "#232530" :box (:line-width 10 :color "#232530"))))))

(defvar pb-theme_echo-area-face
  '(default (:background "#232530" :box (:line-width 10 :color "#232530"))))

(dolist (buf (list " *Minibuf-0*" " *Minibuf-1*" " *Echo Area 0*" " *Echo Area 1*"))
  (when (get-buffer buf)
    (with-current-buffer buf
      (setq-local face-remapping-alist (list pb-theme_echo-area-face)))))

(add-hook 'minibuffer-setup-hook
      (lambda ()
        (make-local-variable 'face-remapping-alist)
        (add-to-list 'face-remapping-alist pb-theme_echo-area-face)))

(set-face-background 'minibuffer-prompt "grey10")
(+ 1 2)
