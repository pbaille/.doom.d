;;; pb/pb-clojure.el -*- lexical-binding: t; -*-

(require 'cider)
(require 'pb-cider)

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
  (with-current-buffer (or (cider-current-repl-buffer)
                           (pb-cider_select-repl-buffer))
    (let ((response (cider-nrepl-sync-request:eval (or code "nil"))))
      (km :value (nrepl-dict-get response "value")
          :error (nrepl-dict-get response "err")))))

(defun pb-clojure_gptel-tool-function (code)
  (pb-gptel_mk-prompt
   (km :clojure
       (km :expression code
           :evaluation (km_filter (pb-clojure_eval-string code)
                                  (pb_fn [(cons k v)] v))))))

'(:tries
  (pb-clojure_eval-string "(+ 2 3)")
  (pb-clojure_gptel-tool-function "(+ 2 3)"))

(defvar pb-clojure_gptel-tool
  (gptel-make-tool
   :name "eval_clojure"
   :function (lambda (code)
               (pb-gptel_mk-prompt
                (pb-clojure_eval-string code)))
   :description "Evaluates Clojure code in the current CIDER REPL and returns the result."
   :args (list '(:name "code"
                 :type string
                 :description "Clojure code to evaluate"))
   :category "clojure"
   :confirm t))

(defun pb-clojure_gptel-replace ()
  "Evaluate the current Clojure expression and request GPT to analyze it.
This function:
1. Captures the current symbolic expression
2. Evaluates it using the Clojure runtime
3. Sends both the original expression and its evaluation result to GPT
4. Replaces the current expression with GPT's improved version

Useful for debugging and improving Clojure code by letting the language model
see both the code and its runtime behavior at once."
  (interactive)
  (pb-gptel_current-symex-request-replace
   (concat "current expression evaluates to:\n"
           (pb-clojure_eval-string (pb-symex_current-as-string))
           "\nFix it if needed.")))
