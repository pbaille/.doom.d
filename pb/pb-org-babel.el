;;; pb-org-babel.el --- org-babel utils -*- lexical-binding: t; -*-

;; Author: Pierre Baille
;; URL: https://github.com/pbaille
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; org babel utils.

;;; Code:

(defun pb-org-babel_strip-lisp-comments (code)
  "Remove comments from a string of Lisp CODE.
; (semi colon) is assumed to be the inline comment character."
  (replace-regexp-in-string "\\(;.*$\\)" "" code nil nil 1))

(defvar pb-org-babel_custom-params
  (km :clojure
      (km
       :pp (km :args (lambda (args)
                       (append '((:results . "raw pp")
                                 (:wrap . "src clojure"))
                               args))))))

(defun pb-org-babel_add-custom-param (name lang spec)
  (cl-assert (and (keywordp name)
                  (keywordp lang)
                  (km? spec)))
  (setq pb-org-babel_custom-params
        (km_put pb-org-babel_custom-params
                (list lang name)
                spec)))

(defun pb-org-babel_execute-src-block-hook (fun &optional arg info params)
  (pb_if [(list lang content args) info
          custom-params (km_get pb-org-babel_custom-params (pb_keyword lang))
          ;; because of inline comments bug
          content (if (equal "clojure" lang)
                      (pb-org-babel_strip-lisp-comments content)
                    content)
          (cons content args) (seq-reduce (pb_fn [(cons content args) (cons k wrappers)]
                                                 (if (assoc k args)
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
          ;; because of inline comment bug
          content (if (equal "clojure" lang)
              (pb-org-babel_strip-lisp-comments content)
            content)
          custom-params (km_get pb-org-babel_custom-params (pb_keyword lang))]
         (pb_if [result (seq-reduce (pb_fn [result (cons k wrappers)]
                                           (if (assoc k args)
                                               (pb_if [f (km_get wrappers :result)]
                                                      (funcall f result)
                                                      result)
                                             result))
                                    (km_entries custom-params)
                                    result)]
                (apply fun result
                       result-params
                       info
                       more))
         (apply fun result result-params info more)))

(advice-add 'org-babel-insert-result :around #'pb-org-babel_insert-result-hook)

(provide 'pb-org-babel)
;;; pb-org-babel.el ends here.
