;;; pb-all.el --- All pb packages -*- lexical-binding: t; -*-

;; Author: Pierre Baille
;; URL: https://github.com/pbaille
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; require every pb packages.

;;; Code:

(print "pb loading..")

(require 'sq)
(require 'km)
(require 'pb)
(require 'pb-destructure)
(require 'pb-flow)
(require 'pb-tree)

(require 'pb-lisp)
(require 'pb-cider)
(require 'pb-color)
(require 'pb-dired)
(require 'pb-elisp)
(require 'pb-fennel)
(require 'pb-fold)
(require 'pb-gptel)
(require 'pb-llm)
(require 'pb-ibuffer)
(require 'pb-misc)
(require 'pb-org)
(require 'pb-org-babel)
(require 'pb-org-clojure)
(require 'pb-reapl)
(require 'pb-sexpr)
(require 'pb-symex)
(require 'pb-templates)
(require 'pb-text-properties)
(require 'pb-udp)
(require 'pb-walk)

(require 'sorg)

(print "pb loaded")
(provide 'pb-all)
;;; pb-all.el ends here.
