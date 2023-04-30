;;; pb/names.el -*- lexical-binding: t; -*-

(defun symbol-to-keyword (sym)
  "Convert the symbol SYM to a keyword."
  (intern (concat ":" (symbol-name sym))))

(cl-assert (and (equal :iop (symbol-to-keyword 'iop))
                (keywordp (symbol-to-keyword 'iop))))

(provide 'names)
