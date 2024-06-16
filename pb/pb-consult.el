;;; pb/pb-consult.el -*- lexical-binding: t; -*-

(require 'consult)

(progn "put source buffer before special buffers in the consult propositions"

       (defun consult--buffer-sort-file-first (buffers)
         (sort buffers (lambda (a b)
                         (let ((file-a (buffer-file-name a))
                               (file-b (buffer-file-name b)))
                           (if file-a
                               (if file-b
                                   ;; If both buffers visit files, sort alphabetically.
                                   (< (or (seq-position buffers a) most-positive-fixnum)
                                      (or (seq-position buffers b) most-positive-fixnum))
                                 ;; Buffer A visits a file, but buffer B doesn't.
                                 t)
                             ;; Buffer A doesn't visit a file.
                             (if file-b
                                 ;; Buffer B visits a file, so buffer A goes at the end.
                                 nil
                               ;; Neither buffer visits a file, so sort alphabetically.
                               (string-lessp (buffer-name a) (buffer-name b))))))))

       (setq consult--source-buffer
             `(:name     "Buffer"
               :narrow   (?b . "Buffer")
               ;;:hidden   t
               :history  buffer-name-history
               :category buffer
               :state    ,#'consult--buffer-state
               ;; The buffer source must come first, because the `consult--multi' completion
               ;; system gives the candidates from the first source which returns non-nil
               ;; as the "initial input". For `consult-buffer' this should be the `consult--source-buffer'.
               :default  (lambda () (buffer-name (consult--buffer-query)))
               :items
               ,(lambda () (consult--buffer-query :sort 'file-first :as #'buffer-name)))))
