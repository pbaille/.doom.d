;;; pb/xp/org-babel-execute.el -*- lexical-binding: t; -*-

(defvar pb-org-babel_custom-params
  (km :clojure
      (km
       :pp (km :args (lambda (args)
                       (append '((:results . "raw pp")
                                 (:wrap . "src clojure"))
                               args)))

       :proll (km :content (lambda (content)
                             (format "((requiring-resolve 'noon.doc.utils/->piano-roll) %s)"
                                     content))
                  :args (lambda (args)
                          (pb->> args
                                 (assq-delete-all :results)
                                 (assq-delete-all :result-params)
                                 (append '((:results . "none")
                                           (:result-params "none")))))
                  :result (lambda (result)
                            (pp (cons :result! result))
                            result)))))

(defun pb-org-babel_execute-src-block-hook (fun &optional arg info params)
  (pb_if [(list lang content args) info
          custom-params (km_get pb-org-babel_custom-params (pb_keyword lang))
          (cons content args) (seq-reduce (pb_fn [(cons content args) (cons k wrappers)]
                                                 (if (alist-get k args)
                                                     (cons (pb_if [f (km_get wrappers :content)]
                                                                  (funcall f content)
                                                                  content)
                                                           (pb_if [f (km_get wrappers :args)]
                                                                  (funcall f args)
                                                                  args))
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
          (cons result args) (seq-reduce (pb_fn [(cons result args) (cons k wrappers)]
                                                 (if (alist-get k args)
                                                     (cons (pb_if [f (km_get wrappers :result)]
                                                                  (funcall f result)
                                                                  result)
                                                           (pb_if [f (km_get wrappers :args)]
                                                                  (funcall f args)
                                                                  args))
                                                   (cons content args)))
                                          (km_entries custom-params)
                                          (cons result args))]
         (apply fun result
                (or (alist-get :result-params args)
                    result-params)
                (sq_put info 2 args)
                more)
    (apply fun result result-params info more)))

(advice-add 'org-babel-insert-result :around #'pb-org-babel_insert-result-hook)
