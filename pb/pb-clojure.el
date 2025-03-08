;;; pb/pb-clojure.el -*- lexical-binding: t; -*-

(require 'cider)

(defun pb-clojure_thing-at-point-tagged-literal ()
  "Return the Clojure tagged literal at point, or nil if none is found."
  (let ((bounds (bounds-of-thing-at-point 'sexp)))
    (when bounds
      (if (save-excursion
            (goto-char (car bounds))
            (looking-at "#\\w+"))
          (buffer-substring-no-properties (car bounds) (cdr bounds))
        nil))))

(defun pb-clojure_thing-at-point ()
  (or (pb-clojure_thing-at-point-tagged-literal)
      (thing-at-point 'sexp)
      (thing-at-point 'symbol)))

(defun pb-clojure_eval-string (code)
  (when (not code)
    (setq code "nil"))
  (when (not (cider-current-repl))
    (error "No active CIDER REPL found"))
  (condition-case err
      (with-current-buffer (cider-current-repl)
        (let* ((response (cider-nrepl-sync-request:eval code))
               (result (nrepl-dict-get response "value"))
               (err (nrepl-dict-get response "err")))
          (if err
              (format "Error: %s" err)
            result)))
    (error (format "Error: %S" err))))
