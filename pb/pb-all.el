;;; pb-all.el --- All pb packages -*- lexical-binding: t; -*-

;; Author: Pierre Baille
;; URL: https://github.com/pbaille
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; require every pb packages.

;;; Code:

(print "pb loading..")

(require 'pb-base "./base/all.el")
(require 'pb-utils "./utils/all.el")
(require 'pb-languages "./languages/all.el")
(require 'pb-modes "./modes/all.el")
(require 'pb-llms "./llms/all.el")

(require 'pb-color)
(require 'pb-misc)
(require 'pb-reapl)

(print "pb loaded")
(provide 'pb-all)
;;; pb-all.el ends here.
