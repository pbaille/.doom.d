;;; config/misc.el -*- lexical-binding: t; -*-

(setq warning-minimum-log-level :error)

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

;; pop-up-frames: If you want new buffers to always appear in the current window (unless another window is specifically requested), you can set this variable to nil
(setq pop-up-frames nil)

(progn :shorthand-bug-fix

       "In my current version (emacs-mac) the 'elisp-shorthand-font-lock-face was taking an extra char."

       (defun shorthands-font-lock-shorthands (limit)
         (when read-symbol-shorthands
           (while (re-search-forward
                   (concat "\\_<\\(" (rx lisp-mode-symbol) "\\)\\_>")
                   limit t)
             (let* ((existing (get-text-property (match-beginning 1) 'face))
                    (probe (and (not (memq existing '(font-lock-comment-face
                                                      font-lock-string-face)))
                                (intern-soft (match-string 1))))
                    (sname (and probe (symbol-name probe)))
                    (mm (and sname (shorthands--mismatch-from-end
                                    (match-string 1) sname))))
               (unless (or (null mm) (= mm (length sname)))
                 (add-face-text-property (match-beginning 1) (- (match-end 1) mm)
                                         'elisp-shorthand-font-lock-face))))))

       (font-lock-add-keywords 'emacs-lisp-mode '((shorthands-font-lock-shorthands)) t))
