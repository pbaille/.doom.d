;;; config/misc.el -*- lexical-binding: t; -*-

(setq user-script-dir
      (concat doom-user-dir "pb/"))

;; extend the loading path to include my elisp dir
(add-to-list 'load-path user-script-dir)

;; loading secret stuff
(load "secrets")

(setq initial-frame-alist
      '((top . 0) (left . 0)
        (width . 200) (height . 150)))

;;(setq display-line-numbers-type 'relative)
(setq display-line-numbers-type nil)
(auto-dim-other-buffers-mode 0)
(setq max-lisp-eval-depth 6400)
;; hack given by doom creator for a workspace loading issue if i remember well
(defadvice! enforce-encoding-a
  (fn &rest args)
  :around #'persp-list-persp-names-in-file
  :around #'persp-load-state-from-file
  :around #'persp-save-state-to-file
  (let ((coding-system-for-read 'utf-8)
        (coding-system-for-write 'utf-8))
    (apply fn args)))

(set-popup-rules!
  '(("^\\*Messages" :ttl t :quit t)))

(add-hook 'prog-mode-hook
          (lambda () (reveal-mode 1)))

(setq ns-function-modifier 'hyper)

(setq mouse-wheel-scroll-amount
      '(1
        ((shift)
         . hscroll)))
