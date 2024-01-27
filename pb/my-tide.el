;;; pb/my-tide.el -*- lexical-binding: t; -*-

(require 'tide)

(defun files-filter (parent-dir files)
  "Filter the relevant PROJECTS to include only the desired subproject."
  (cl-remove-if-not (lambda (f)
                      (string-prefix-p parent-dir f))
                    files))

(setq my-tide-directory-filter "")

(defun tide-filenames-filter (files)
  (files-filter
   my-tide-directory-filter
   files))
