;;; pb-text-properties.el --- text properties utils -*- lexical-binding: t; -*-

;; Author: Pierre Baille
;; URL: https://github.com/pbaille
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Text properties utils.

;;; Code:

(require 'km)

(defun pb-text-properties_get-changes (buf start end)
  "Return a list of text property changes for BUF between START and END.
The change list returned has the form ((pos . face-prop-value) ...)."
  (with-current-buffer (or buf (current-buffer))
    (goto-char start)
    (let ((ret ()))
      (while (< (point) end)
        (let ((face-prop (plist-get (text-properties-at (point))
                                    'face))
              (next-change
               (or (next-single-property-change (point) 'face (current-buffer))
                   (point-max))))
          (setq ret (cons (cons (point) face-prop) ret))
          (goto-char next-change)))
      ret)))

(defun pb-text-properties_compress-face-value (v)
  "Compress the value V of a face property into a plist."
  (if (and (consp v) (listp (car v)))
      (seq-reduce #'km_merge (reverse v) ())
    (copy-tree v)))

(defun pb-text-properties_update-face-value (f v)
  "Apply the function F to a face property value V.
Face prop can be nil, a plist, or a list of plists,
therefore F has to be applied meaninfully."
  (funcall f (pb-text-properties_compress-face-value v)))

(defun pb-text-properties_update-faces (buf start end f)
  "Apply F to every face properties between START end END in BUF."
  (with-current-buffer (or buf (current-buffer))
    (goto-char start)
    (while (< (point) end)
      (let ((face-prop (plist-get (text-properties-at (point))
                                  'face))
            (next-change
             (or (next-single-property-change (point) 'face (current-buffer))
                 (point-max))))
        (print (list (point) (min next-change end)
                     (pb-text-properties_update-face-value f face-prop)) )
        (add-face-text-property (point) (min next-change end)
                                (pb-text-properties_update-face-value f face-prop)
                                nil
                                (or buf (current-buffer))
                                )
        (goto-char next-change)))))

(quote
 (list
  (with-current-buffer (get-buffer-create "*delete-me*")
    (erase-buffer)
    (insert "hello you ---------------"))
  (pb-text-properties_get-changes (get-buffer "*delete-me*") 3 13)
  (pb-text-properties_update-faces (get-buffer "*delete-me*") 3 13 (lambda (pl) (km_upd pl :background (lambda (c) (pb-color (or c :white) (lighten .5))))))
  (pb-text-properties_update-faces (get-buffer "*delete-me*")
                                   2 10
                                   (lambda (pl) (pb-color_walk pl (lambda (c) (pb-color_blend (pb-color :blue) c .5)))))
  (with-current-buffer (get-buffer-create "*delete-me*")
    (erase-buffer)
    (insert (propertize "hello you ---------------"
                        'face
                        (list :background (pb-color "gray80"))))
    (pb-text-properties_update-faces (get-buffer "*delete-me*")
                                     3 6 (lambda (pl)
                                           '(print (km :pl pl
                                                       :upd (km_upd pl :background (lambda (c) (pb-color c (lighten .5))))))
                                           (km_upd pl :background (lambda (c) "red"))))
    '(pb-text-properties_update-faces (get-buffer "*delete-me*")
      3 6 (lambda (pl) (list :background (pb-color (or (km_get pl :background)
                                                       "white")
                                                   (darken .3)))))
    '(add-face-text-property 3 6
      (list :background "white")
      nil
      (get-buffer "*delete-me*")))))

(provide 'pb-text-properties)
;;; pb-text-properties.el ends here.
