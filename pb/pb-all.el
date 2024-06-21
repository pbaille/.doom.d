;;; pb-all.el --- All pb packages -*- lexical-binding: t; -*-

;; Author: Pierre Baille
;; URL: https://github.com/pbaille
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; require every pb packages.

;;; Code:

(print "pb loading..")

(require 'pb)
(require 'km)
(require 'sq)

(require 'pb-cider)
(require 'pb-color)
(require 'pb-dired)
(require 'pb-elisp)
(require 'pb-fennel)
(require 'pb-fold)
(require 'pb-gptel)
(require 'pb-ibuffer)
(require 'pb-misc)
(require 'pb-reapl)
(require 'pb-sexpr)
(require 'pb-symex)
(require 'pb-templates)
(require 'pb-udp)
(require 'pb-walk)

(print "pb loaded")
(provide 'pb-all)
;;; pb-all.el ends here.
