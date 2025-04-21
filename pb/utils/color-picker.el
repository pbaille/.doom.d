;;; color-picker.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 pierre baille
;;
;; Author: pierre baille <https://github.com/pierrebaille>
;; Maintainer: pierre baille <pierrebaille@MBP2>
;; Created: April 26, 2022
;; Modified: April 26, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/pierrebaille/color-picker
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:
(require 'core)
(require 'core-keybinds)
(require 'kurecolor)
(require 'hydra)

(defun color-picker-reload ()
  "Refresh ayu theme."
  (interactive)
  (save-buffer (current-buffer))
  (load-theme 'doom-ayu-light-tweaked t)
  ;; (load-theme 'doom-nord t)
  )

(map! :leader
      "t k"
      (defhydra hydra-toggle-simple
        (:pre
         (progn (hl-line-mode -1)
                (rainbow-mode 1))
         :post
         (progn (hl-line-mode 1)
                (rainbow-mode -1)))
        "color edition utility"
        ("l" kurecolor-increase-hue-by-step "hue+")
        ("h" kurecolor-decrease-hue-by-step "hue-")
        ("o" kurecolor-increase-saturation-by-step "sat+")
        ("y" kurecolor-decrease-saturation-by-step "sat-")
        ("i" kurecolor-increase-brightness-by-step "bri+")
        ("u" kurecolor-decrease-brightness-by-step "bri-")
        ("j" (lambda () (interactive) (search-forward "\"#" nil t) (backward-char)) "next color")
        ("k" (lambda () (interactive) (search-backward "\"#" nil t) (forward-char)) "prev color")
        ("r" color-picker-reload "reload")
        ("RET" color-picker-reload "done" :color blue)))


(provide 'color-picker)
;;; color-picker.el ends here
