;;; themes/scratch.el -*- lexical-binding: t; -*-

(require 'km)
(require 'pb-color)

(pb-color "red"
          (desaturate .4)
          (lighten .5)
          (hue-wheel 6))

(let ((base (pb-color (pb-color_random)
                      (desaturate .4))))
  (list (pb-color_darken base 0.6)
        (pb-color_lighten (pb-color_complementary base) 0.6)))

(defun rand-color ()
  (pb-color_random))

(let ((c (rand-color)))
  (list c
        (pb-color c
                  (complementary)
                  (set-lightness (- 1 (pb-color_lightness c))))))

(defun color-walk (data f)
  ""
  (cond ((consp data)
         (mapcar (lambda (x) (color-walk x f))
                 data))
        ((stringp data)
         (funcall f data))
        (t data)))

(defun pprob (x)
  (pp x)
  nil)

(defmacro fn (args &rest body)
  "Like lambda but with a shorter name and destructuring."
  (let ((bindings (cl-loop for i from 0 to (- (length args) 1)
                           collect (cons (gensym (format "arg_%s" i)) (nth i args)))))
    `(lambda ,(mapcar #'car bindings)
       ,(seq-reduce (lambda (ret x)
                      `(cl-destructuring-bind ,(cdr x) ,(car x)
                         ,ret))
                    (reverse bindings)
                    (cons 'progn body)))))


(defun pb-theme_palette (fg)
  "Derive a theme palette from a foreground color FG."
  (let* ((bg (pb-color_complementary fg))

         (decline (lambda (x)
                    (let* ((dimed (pb-color_neutralize-lightness x .1))
                           (vivid (pb-color_exacerbate-lightness x .3))
                           (alt (pb-color_update-hue dimed (lambda (x) (+ .1 x))))
                           (alt2 (pb-color_update-hue dimed (lambda (x) (- x .1)))))
                      (list :main x
                            :dim dimed
                            :vivid vivid
                            :alt alt
                            :alt2 alt2
                            :accent (pb-color_complementary-hue x)))))

         (colors (km_into ()
                          (seq-mapn #'cons
                                    (list :yellow :green :cyan :blue :magenta :red )
                                    (pb-color fg
                                              (set-hue 0)
                                              (set-saturation .8)
                                              (hue-wheel 6)))))

         (decline-color
          (lambda (c)
            (let ((fg (list :main c
                            :warmer (pb-color_warm c .1)
                            :cooler (pb-color_cool c .1)
                            :faint (pb-color c
                                             (desaturate .7)
                                             (neutralize-lightness .5)))))
              (list :fg fg
                    :bg (color-walk fg #'pb-color_complementary-lightness))))))

    (list :bg (funcall decline bg)
          :fg (funcall decline fg)
          :colors (km_map
                   colors
                   (fn ((name . c))
                       (cons name (funcall decline-color c)))))))

(defun km_flat (m sep)
  (seq-mapcat (fn ((k . v))
                  (if (km? v)
                      (km_map (km_flat v sep)
                              (fn ((k1 . v1))
                                  (cons (intern
                                         (concat ":"
                                                 (pb_keyword-name k)
                                                 sep
                                                 (pb_keyword-name k1)))
                                        v1)))
                    (list k v)))
              (km_entries m)))


(progn :macros

       (defmacro -> (x &rest forms)
         (seq-reduce
          (lambda (result form)
            (if (seqp form)
                `(,(car form) ,result ,@(cdr form))
              (list form result)))
          forms
          x))

       (defmacro dlet (bindings &rest body)
         (seq-reduce
          (lambda (ret binding)
            (cl-destructuring-bind (pat expr) binding
              (if (symbolp pat)
                  `(let (,binding) ,ret)
                `(cl-destructuring-bind ,@binding ,ret))))
          (reverse (sq_partition 2 2 bindings))
          (cons 'progn body)))

       (defmacro >_ (&rest forms)
         "Thread the first argument into following FORMS.
using the _ placeholder to determine threaded value positioning."
         (cl-destructuring-bind (ret . bindings) (reverse forms)
           `(let* ,(seq-reduce
                    (lambda (bindings form)
                      (cons (list '_ form) bindings))
                    bindings
                    (list))
              ,ret)))

       (defmacro f_ (&rest body)
         `(lambda (_)
            ,@body)))

(defun pb-theme_transform-color-key (k separator)
  (pcase (string-split (symbol-name k) separator)
    (`(":colors" . ,xs)
     (intern (concat ":"
                     (pcase xs
                       (`(,color "fg" "main") color)
                       (`(,color "fg" ,variant) (concat color "-" variant))
                       (`(,color "bg" ,variant) (concat "bg-" color "-"
                                                        (pcase variant
                                                          ("main" "intense")
                                                          ("faint" "subtle")
                                                          (_ variant))))))))))

(defun pb-theme_rand-palette ()
  (-> (pb-color_from-hsl
       (list (pb-color_random-value) .2 .8))
      (pb-theme_palette)
      (km_flat "-")
      (km_map-keys
       (f_ (pb_keyword-to-symbol
            (or (pb-theme_transform-color-key _ "-") _))))
      (km_map-vals #'list)
      (km_entries))

  )

(pprob (let* ((palette (pb-theme_palette (pb-color_from-hsl
                                          (list (pb-color_random-value) .5 .8))))

              (light (color-walk palette #'pb-color_complementary-lightness)))

         (list :dark palette
               :light light)))
