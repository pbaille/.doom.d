;;; themes/scratch.el -*- lexical-binding: t; -*-

(require 'km)
(require 'pb-color)

(pb-color "red"
          (desaturate .4)
          (lighten .5)
          (hue-wheel 6))

("#e4e498" "#98e498" "#98e4e4" "#9898e4" "#e498e4" "#e49898")


(let ((base (pb-color (pb-color_random)
                      (desaturate .4))))
  (list (pb-color_darken base 0.6)
        (pb-color_lighten (pb-color_complementary base) 0.6)))

("#1c3c37" "#e2c3c7")

(defun rand-color ()
  (pb-color_random))

(let ((c (rand-color)))
  (list c
        (pb-color c
                  (complementary)
                  (set-lightness (- 1 (pb-color_lightness c))))))

("#a45243" "#5aadbc")

("#1cf8f8" "#e30707")

("#52d9e9" "#ac2616")

("#60f9a7" "#9e0657")

("#78de94" "#87206a")

("#cfdc73" "#30228c")

(defun color-walk (data f)
  ""
  (cond ((consp data)
         (mapcar (lambda (x) (color-walk x f))
                 data))
        ((stringp data)
         (funcall f data))
        (t data)))

(defun pb-palette (base)
  "Generate a color palette based on BASE (hex color)."
  (let ((colors (pb-color_overview base)))
    (list :light
          (list :fg
                :bg )

          :dark
          (list ))))

(defun pprob (x)
  (pp x)
  nil)

(defmacro fn (args &rest body)
  (let ((bindings (cl-loop for i from 0 to (- (length args) 1)
                           collect (cons (gensym (format "arg_%s" i)) (nth i args)))))
    `(lambda ,(mapcar #'car bindings)
       ,(seq-reduce (lambda (ret x)
                      `(cl-destructuring-bind ,(cdr x) ,(car x)
                         ,ret))
                    (reverse bindings)
                    (cons 'progn body)))))

(pprob (let* ((bg (pb-color_from-hsl
                   (list (pb-color_random-value) .5 .2)))

              (fg (pb-color_complementary bg))

              (decline (lambda (x)
                         (let* ((dimed (pb-color_neutralize-lightness x .1))
                                (alt (pb-color_update-hue dimed (lambda (x) (+ .1 x)))))
                           (list :main x
                                 :dim dimed
                                 :alt alt))))

              (colors (km_into ()
                               (seq-mapn #'cons
                                         (list :yellow :green :cyan :blue :magenta :red )
                                         (pb-color fg
                                                   (set-hue 0)
                                                   (set-saturation .7)
                                                   (hue-wheel 6)))))

              (decline-color
               (lambda (c)
                 (let ((fg (list :main c
                                 :warmer (pb-color_warm c .1)
                                 :cooler (pb-color_cool c .1)
                                 :faint (pb-color_neutralize-lightness (pb-color_desaturate c .7)
                                                                       .5))))
                   (list :fg fg
                         :bg (color-walk fg #'pb-color_complementary-lightness)))))

              (dark (list :bg (funcall decline bg)
                          :fg (funcall decline fg)
                          :colors (km_map
                                   colors
                                   (fn ((name . c))
                                       (cons name (funcall decline-color c))))))

              (light (color-walk dark #'pb-color_complementary-lightness)))

         (list :dark dark
               :light light)))
(:dark
 (:bg
  (:main "#4c4319" :dim "#574d1c" :alt "#3d571b")
  :fg
  (:main "#b3bce6" :dim "#a7b1e2" :alt "#c0a6e2")
  :colors
  (:yellow
   (:fg
    (:main "#efefa9" :warmer "#f0e698" :cooler "#d7d7b1" :faint "#b7b793")
    :bg
    (:main "#555510" :warmer "#675d0f" :cooler "#4e4e28" :faint "#6c6c48"))
   :green
   (:fg
    (:main "#a9efa9" :warmer "#b1e698" :cooler "#98d7b1" :faint "#93b793")
    :bg
    (:main "#105510" :warmer "#316718" :cooler "#286741" :faint "#486c48"))
   :cyan
   (:fg
    (:main "#a9efef" :warmer "#b1e6d7" :cooler "#98d7f0" :faint "#93b7b7")
    :bg
    (:main "#105555" :warmer "#184d3e" :cooler "#0f4d67" :faint "#486c6c"))
   :blue
   (:fg
    (:main "#a9a9ef" :warmer "#b1a7d7" :cooler "#9898f0" :faint "#9393b7")
    :bg
    (:main "#101055" :warmer "#312858" :cooler "#0f0f67" :faint "#48486c"))
   :magenta
   (:fg
    (:main "#efa9ef" :warmer "#f0a7d7" :cooler "#d798f0" :faint "#b793b7")
    :bg
    (:main "#551055" :warmer "#570f3e" :cooler "#4e0f67" :faint "#6c486c"))
   :red
   (:fg
    (:main "#efa9a9" :warmer "#f0a798" :cooler "#d798b1" :faint "#b79393")
    :bg
    (:main "#551010" :warmer "#671e0f" :cooler "#672841" :faint "#6c4848"))))
 :light
 (:bg
  (:main "#e6ddb3" :dim "#e3d8a8" :alt "#cae3a8")
  :fg
  (:main "#18214b" :dim "#1d2658" :alt "#371d59")
  :colors
  (:yellow
   (:fg
    (:main "#555510" :warmer "#675d0f" :cooler "#4e4e28" :faint "#6c6c48")
    :bg
    (:main "#efefaa" :warmer "#efe598" :cooler "#d6d7b0" :faint "#b7b793"))
   :green
   (:fg
    (:main "#105510" :warmer "#316718" :cooler "#286741" :faint "#486c48")
    :bg
    (:main "#aaefaa" :warmer "#b1e798" :cooler "#98d6b1" :faint "#93b793"))
   :cyan
   (:fg
    (:main "#105555" :warmer "#184d3e" :cooler "#0f4d67" :faint "#486c6c")
    :bg
    (:main "#aaefef" :warmer "#b2e7d8" :cooler "#98d5ef" :faint "#93b7b7"))
   :blue
   (:fg
    (:main "#101055" :warmer "#312858" :cooler "#0f0f67" :faint "#48486c")
    :bg
    (:main "#aaaaef" :warmer "#b0a7d7" :cooler "#9898ef" :faint "#9393b7"))
   :magenta
   (:fg
    (:main "#551055" :warmer "#570f3e" :cooler "#4e0f67" :faint "#6c486c")
    :bg
    (:main "#efaaef" :warmer "#f0a8d6" :cooler "#d798ef" :faint "#b793b7"))
   :red
   (:fg
    (:main "#551010" :warmer "#671e0f" :cooler "#672841" :faint "#6c4848")
    :bg
    (:main "#efaaaa" :warmer "#efa798" :cooler "#d698b1" :faint "#b79393")))))
nil

(:dark
 (:bg
  (:main "#193b4c" :dim "#1c4357" :alt "#1b1f57" :colors
         (:yellow
          (:fg
           (:main "#efefa9" :warmer "#f0e698" :cooler "#d7d7b1" :faint "#b7b793")
           :bg
           (:main "#555510" :warmer "#675d0f" :cooler "#4e4e28" :faint "#6c6c48"))
          :green
          (:fg
           (:main "#a9efa9" :warmer "#b1e698" :cooler "#98d7b1" :faint "#93b793")
           :bg
           (:main "#105510" :warmer "#316718" :cooler "#286741" :faint "#486c48"))
          :cyan
          (:fg
           (:main "#a9efef" :warmer "#b1e6d7" :cooler "#98d7f0" :faint "#93b7b7")
           :bg
           (:main "#105555" :warmer "#184d3e" :cooler "#0f4d67" :faint "#486c6c"))
          :blue
          (:fg
           (:main "#a9a9ef" :warmer "#b1a7d7" :cooler "#9898f0" :faint "#9393b7")
           :bg
           (:main "#101055" :warmer "#312858" :cooler "#0f0f67" :faint "#48486c"))
          :magenta
          (:fg
           (:main "#efa9ef" :warmer "#f0a7d7" :cooler "#d798f0" :faint "#b793b7")
           :bg
           (:main "#551055" :warmer "#570f3e" :cooler "#4e0f67" :faint "#6c486c"))
          :red
          (:fg
           (:main "#efa9a9" :warmer "#f0a798" :cooler "#d798b1" :faint "#b79393")
           :bg
           (:main "#551010" :warmer "#671e0f" :cooler "#672841" :faint "#6c4848"))))
  :fg
  (:main "#e6c3b3" :dim "#e2b9a7" :alt "#e2dca6"))
 :light
 (:bg
  (:main "#b3d5e6" :dim "#a8cee3" :alt "#a8ace3" :colors
         (:yellow
          (:fg
           (:main "#555510" :warmer "#675d0f" :cooler "#4e4e28" :faint "#6c6c48")
           :bg
           (:main "#efefaa" :warmer "#efe598" :cooler "#d6d7b0" :faint "#b7b793"))
          :green
          (:fg
           (:main "#105510" :warmer "#316718" :cooler "#286741" :faint "#486c48")
           :bg
           (:main "#aaefaa" :warmer "#b1e798" :cooler "#98d6b1" :faint "#93b793"))
          :cyan
          (:fg
           (:main "#105555" :warmer "#184d3e" :cooler "#0f4d67" :faint "#486c6c")
           :bg
           (:main "#aaefef" :warmer "#b2e7d8" :cooler "#98d5ef" :faint "#93b7b7"))
          :blue
          (:fg
           (:main "#101055" :warmer "#312858" :cooler "#0f0f67" :faint "#48486c")
           :bg
           (:main "#aaaaef" :warmer "#b0a7d7" :cooler "#9898ef" :faint "#9393b7"))
          :magenta
          (:fg
           (:main "#551055" :warmer "#570f3e" :cooler "#4e0f67" :faint "#6c486c")
           :bg
           (:main "#efaaef" :warmer "#f0a8d6" :cooler "#d798ef" :faint "#b793b7"))
          :red
          (:fg
           (:main "#551010" :warmer "#671e0f" :cooler "#672841" :faint "#6c4848")
           :bg
           (:main "#efaaaa" :warmer "#efa798" :cooler "#d698b1" :faint "#b79393"))))
  :fg
  (:main "#4b2818" :dim "#582f1d" :alt "#59531d")))
nil
