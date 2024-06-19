;;; pb-all.el --- All pb packages -*- lexical-binding: t; -*-

;; Author: Pierre Baille
;; URL: https://github.com/pbaille
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Load every pb packages.

;;; Code:

(load "~/.doom.d/pb/pb.el")

(load "~/.doom.d/pb/km.el")
(load "~/.doom.d/pb/sq.el")

(load "~/.doom.d/pb/pb-cider.el")
(load "~/.doom.d/pb/pb-symex.el")
(load "~/.doom.d/pb/pb-dired.el")
(load "~/.doom.d/pb/pb-elisp.el")
(load "~/.doom.d/pb/pb-fennel.el")
(load "~/.doom.d/pb/pb-fold.el")
(load "~/.doom.d/pb/pb-gptel.el")
(load "~/.doom.d/pb/pb-ibuffer.el")
(load "~/.doom.d/pb/pb-misc.el")
(load "~/.doom.d/pb/pb-reapl.el")
(load "~/.doom.d/pb/pb-sexpr.el")
(load "~/.doom.d/pb/pb-sidebars.el")
(load "~/.doom.d/pb/pb-templates.el")
(load "~/.doom.d/pb/pb-udp.el")
(load "~/.doom.d/pb/pb-walk.el")

(provide 'pb-all)
;;; pb-all.el ends here.
