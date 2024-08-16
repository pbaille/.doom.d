;;; scratch.el -*- lexical-binding: t; -*-

(setq org-modern-table nil)
(setq org-src-window-setup 'current-window)

(setq symex-refocus-p nil)
(setq lsp-enable-symbol-highlighting nil)
(setq clojure-pr )

(defun pb-clojure-adaptive-fill-function ()
  "Clojure adaptive fill function.
This only takes care of filling docstring correctly."
  (when (clojure-in-docstring-p)
    (pp "io")
    (make-string (save-excursion
                   (beginning-of-thing 'string)
                   (- (point)
                      (save-excursion (beginning-of-line) (point))))
                 ? )))

(setq-local adaptive-fill-function
            #'pb-clojure-adaptive-fill-function)
