;;; themes/scratch.el -*- lexical-binding: t; -*-

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

(pprob (let* ((bg (pb-color_from-hsl (list (pb-color_random-value)
                                           .5
                                           .2)))
              (complementary-lightness (lambda (x)
                                         (pb-color_set-lightness x
                                                                 (- 1 (pb-color_lightness x)))))

              (dim (lambda (x by)
                     (if (> (pb-color_lightness x) 0.5)
                         (pb-color_darken x by)
                       (pb-color_lighten x by))))

              (alt (lambda (x by)
                     (pb-color_set-hue x
                                       (mod (+ by (pb-color_hue x)) 1))))

              (decline (lambda (x)
                         (let ((dimed (funcall dim x .1)))
                           (list :main x
                                 :dim dimed
                                 :alt (funcall alt dimed .2)))))

              (fg (pb-color_complementary bg))

              (dark (list :bg (funcall decline bg)
                          :fg (funcall decline fg)))
              (light (color-walk dark complementary-lightness)))
         (list :dark dark
               :light light)))
(:dark
 (:bg
  (:main "#44194c" :dim "#56305d" :alt "#5d3230")
  :fg
  (:main "#bbe6b3" :dim "#a8cfa1" :alt "#a0cccf"))
 :light
 (:bg
  (:main "#deb3e6" :dim "#c8a2cf" :alt "#cfa3a2")
  :fg
  (:main "#204b18" :dim "#375e30" :alt "#305c5f")))


()
