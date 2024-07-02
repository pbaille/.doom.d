;;; pb/xp/org-babel-execute.el -*- lexical-binding: t; -*-

(quote
 (list
  :org-insert-propertized-str
  (list :solution1
        (require 'find-func)
        (with-current-buffer
            (find-file-noselect (find-library-name "ob-core.el"))
          ;; This is a hack.
          ;; `org-no-properties' is defined by `defsubst' and the byte-compiler replaces the symbol with the lambda.
          ;; We need the definition of `org-babel-insert-result' containing the symbol `org-no-properties'.
          ;; Therefore, we eval the defun for `org-babel-insert-result' in the source file.
          (goto-char (point-min))
          (re-search-forward "(defun org-babel-insert-result")
          (eval-defun nil))

        (defun org-no-properties (s &optional _)
          (print "org no properties overload")
          s)

        (defun org+-babel-insert-result-with-props (fun result result-params &rest args)
          "Avoid removing text properties in `org-babel-insert-result'.

Add the new result type \"raw-props\" to Org source blocks.
With this result type text properties are not removed from the result.

This is an :around advice for `org-babel-insert-result' as FUN.
RESULT, RESULT-PARAMS, and ARGS are passed to FUN."
          (print (list result result-params args))
          (if (or (member "props" result-params)
                  (member "properties" result-params))
              (cl-letf* (((symbol-function 'org-no-properties)
                          (lambda (str &rest _args) str)))
                (print "yeah")
                (apply fun result
                       (delete "properties" (remove "props" result-params))
                       args))
            (apply fun result result-params args)))

        (advice-add 'org-babel-insert-result :around #'org+-babel-insert-result-with-props))


  (list :solution2
        (defun org-babel-execute-with-text-properties (fun &optional arg info params)
          "Handler for header argument :text-properties.
Intended as :around advice for `org-babel-execute-src-block'.
Calls FUN with ARG, INFO and PARAMS.
If the alist (org-info- contains an yes- or t-entry for 'text-properties
Function `org-no-properties' is modified to a projection of its string argument."
          (print (list arg info params))
          (setq params (org-combine-plists params (nth 2 info)))
          (if (assoc-string (cdr (assoc-string :text-properties params)) '(yes t))
              (cl-letf (((symbol-function 'org-no-properties) (lambda (str &rest _ignore)
                                                                str)))
                (funcall fun arg info params))
            (funcall fun arg info params)))

        (advice-add 'org-babel-execute-src-block :around #'org-babel-execute-with-text-properties)
        (advice-remove 'org-babel-execute-src-block #'org-babel-execute-with-text-properties))))

(defvar pb-org-babel_custom-params
  (km :clojure
      (km :pp
          (lambda (content args)
            (cons content
                  (append '((:results . "raw pp")
                            (:wrap . "src clojure"))
                          args)))
          :proll
          (lambda (content args)
            (cons (format "((requiring-resolve 'noon.doc.utils/->piano-roll) %s)"
                          content)
                  (pb->> args
                         (assq-delete-all :results)
                         (assq-delete-all :result-params)
                         (append '((:results . "none")
                                   (:result-params "none")))))))))

(defvar pb-org-babel_custom-params2
  (km :clojure
      (km :pp
          (km :args (lambda (args)
                      (append '((:results . "raw pp")
                                (:wrap . "src clojure"))
                              args)))
          :proll
          (km :content (lambda (content)
                         (format "((requiring-resolve 'noon.doc.utils/->piano-roll) %s)"
                                 content))
              :args (lambda (args)
                      (pb->> args
                             (assq-delete-all :results)
                             (assq-delete-all :result-params)
                             (append '((:results . "none")
                                       (:result-params "none")))))
              :result (lambda (result)
                        ())))))

(defun pb-org-babel_execute-src-block-hook (fun &optional arg info params)
  (pb_if [(list lang content args) info
          custom-params (km_get pb-org-babel_custom-params (pb_keyword lang))
          (cons content args) (seq-reduce (pb_fn [(cons content args) (cons k f)]
                                                 (if (alist-get k args)
                                                     (funcall f content args)
                                                   (cons content args)))
                                          (km_entries custom-params)
                                          (cons content args))
          info (pb-> info (sq_put 1 content) (sq_put 2 args))]
         (funcall fun arg info params)
    (funcall fun arg info params)))

(advice-add 'org-babel-execute-src-block :around #'pb-org-babel_execute-src-block-hook)

(defun pb-org-babel_insert-result-hook (fun result result-params info &rest more)
  (pb_if [(list lang content args) info
          custom-params (km_get pb-org-babel_custom-params (pb_keyword lang))
          (cons content args) (seq-reduce (pb_fn [(cons content args) (cons k f)]
                                                 (if (alist-get k args)
                                                     (funcall f content args)
                                                   (cons content args)))
                                          (km_entries custom-params)
                                          (cons content args))]
         (apply fun result
                (or (alist-get :result-params args)
                    result-params)
                (sq_put info 2 args)
                more)
    (apply fun result result-params info more)))

(advice-add 'org-babel-insert-result :around #'pb-org-babel_insert-result-hook)

(quote
 (list :first-experiences

  (defun org-babel-execute_print-args (fun &optional arg info params)
    (let ((opts (nth 2 info)))
      (if (alist-get :ob-spy opts)
          (pp (km :arg arg :info info :params params)))
      (if (alist-get :proll opts)
          ;; add results raw when proll option
          (funcall fun arg (sq_put info
                                   2
                                   (cons (cons :results "raw") opts))
                   params)
        (funcall fun arg info params))))

  (advice-add 'org-babel-execute-src-block :around #'org-babel-execute_print-args)
  (advice-remove 'org-babel-execute-src-block #'org-babel-execute_print-args)

  (defun org-babel-result_print-args (fun result result-params &rest args)
    (pp (km :result result :result-params result-params :args args))
    '(when (pb->_ (car args)
                  (nth 2 _)
                  (alist-get :proll _))
       (print (format "proll: %s" result))
       (print (current-buffer))
       (pb-cider_eval! (format "%s" '(requiring-resolve noon.doc.utils/->piano-roll)))
       (pb-cider_eval! (format "%s" (list 'noon.doc.utils/->piano-roll result))))
    (apply fun result result-params args))

  (advice-add 'org-babel-insert-result :around #'org-babel-result_print-args)
  ))

(quote
 (list :insert-proll-block-ABORTED
  (defvar proll-fontification-buffer (get-buffer-create " *org-src-fontification:proll-mode*"))

  (defun org-src-font-lock-fontify-block_spy (fun &rest args)
    '(pp (km :fontify-spy args
             :mode (org-src-get-lang-mode (car args))))
    (if (equal 'proll-mode (org-src-get-lang-mode (car args)))
        (progn (print "piano-roll")
               (with-current-buffer proll-fontification-buffer
                 (proll-mode -1))
               (let ((ret (apply fun args)))
                 (with-current-buffer proll-fontification-buffer
                   (proll-mode 1))
                 ret))
      (apply fun args)))

  (advice-add 'org-src-font-lock-fontify-block :around #'org-src-font-lock-fontify-block_spy)
  (advice-remove 'org-src-font-lock-fontify-block #'org-src-font-lock-fontify-block_spy)

  (add-to-list 'org-src-lang-modes
               '("piano-roll" . proll))

  (setq org-src-lang-modes
        '(("md" . markdown) ("C" . c) ("C++" . c++) ("asymptote" . asy) ("bash" . sh) ("beamer" . latex) ("calc" . fundamental) ("cpp" . c++) ("ditaa" . artist) ("desktop" . conf-desktop) ("dot" . fundamental) ("elisp" . emacs-lisp) ("ocaml" . tuareg) ("screen" . shell-script) ("shell" . sh) ("sqlite" . sql) ("toml" . conf-toml))
        )

  (with-current-buffer (get-buffer-create " *org-src-fontification:proll-mode*")
    (piano-roll-mode -1)
    (setq piano-roll-data pr-sample-data)
    (piano-roll-mode 1))

  (with-current-buffer proll-fontification-buffer
    (erase-buffer))))
