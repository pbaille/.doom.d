;;; pb/pb-clojure.el -*- lexical-binding: t; -*-

(require 'cider)
(require 'pb-cider)

(defun pb-clojure/thing-at-point-tagged-literal ()
  "Return the Clojure tagged literal at point, or nil if none is found."
  (let ((bounds (bounds-of-thing-at-point 'sexp)))
    (when bounds
      (if (save-excursion
            (goto-char (car bounds))
            (looking-at "#\\w+"))
          (buffer-substring-no-properties (car bounds) (cdr bounds))
        nil))))

(defun pb-clojure/thing-at-point ()
  (or (pb-clojure/thing-at-point-tagged-literal)
      (thing-at-point 'sexp)
      (thing-at-point 'symbol)))

(defun pb-clojure/eval-string (code)
  (with-current-buffer (or (cider-current-repl-buffer)
                           (pb-cider/select-repl-buffer))
    (let ((response (cider-nrepl-sync-request:eval (or code "nil"))))
      (km :value (nrepl-dict-get response "value")
          :error (nrepl-dict-get response "err")))))

(defun pb-clojure/gptel-tool-function (code)
  (pb-prompt/mk
   (km :clojure
       (km :expression code
           :evaluation (km/filter (pb-clojure/eval-string code)
                                  (pb/fn [(cons k v)] v))))))

'(:tries
  (pb-clojure/eval-string "(+ 2 3)")
  (pb-clojure/gptel-tool-function "(+ 2 3)"))

(defun pb-clojure/gptel-replace ()
  "Evaluate the current Clojure expression and request GPT to analyze it.
This function:
1. Captures the current symbolic expression
2. Evaluates it using the Clojure runtime
3. Sends both the original expression and its evaluation result to GPT
4. Replaces the current expression with GPT's improved version

Useful for debugging and improving Clojure code by letting the language model
see both the code and its runtime behavior at once."
  (interactive)
  (pb-gptel/current-symex-request
   (km :prompt (concat "current expression evaluates to:\n"
                       (pb-clojure/eval-string (pb-symex/current-as-string))
                       "\nFix it if needed."))))

(provide 'pb-clojure)
