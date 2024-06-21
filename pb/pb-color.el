;;; pb-color.el --- utils -*- lexical-binding: t; -*-

;; Author: Pierre Baille
;; URL: https://github.com/pbaille
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Color utils.

;;; Code:

(require 'color)

(progn :from-doom

       (defun doom-name-to-rgb (color)
         "Retrieves the hexidecimal string repesented the named COLOR (e.g. \"red\")
for FRAME (defaults to the current frame)."
         (cl-loop with div = (float (car (tty-color-standard-values "#ffffff")))
                  for x in (tty-color-standard-values (downcase color))
                  collect (/ x div)))

       (defun doom-blend (color1 color2 alpha)
         "Blend two colors (hexidecimal strings) together by a coefficient ALPHA (a
float between 0 and 1)"
         (when (and color1 color2)
           (cond ((and color1 color2 (symbolp color1) (symbolp color2))
                  (doom-blend (doom-color color1) (doom-color color2) alpha))

                 ((or (listp color1) (listp color2))
                  (cl-loop for x in color1
                           when (if (listp color2) (pop color2) color2)
                           collect (doom-blend x it alpha)))

                 ((and (string-prefix-p "#" color1) (string-prefix-p "#" color2))
                  (apply (lambda (r g b) (format "#%02x%02x%02x" (* r 255) (* g 255) (* b 255)))
                         (cl-loop for it    in (doom-name-to-rgb color1)
                                  for other in (doom-name-to-rgb color2)
                                  collect (+ (* alpha it) (* other (- 1 alpha))))))

                 (color1))))

       (defun doom-darken (color alpha)
         "Darken a COLOR (a hexidecimal string) by a coefficient ALPHA (a float between
0 and 1)."
         (cond ((and color (symbolp color))
                (doom-darken (doom-color color) alpha))

               ((listp color)
                (cl-loop for c in color collect (doom-darken c alpha)))

               ((doom-blend color "#000000" (- 1 alpha)))))

       (defun doom-lighten (color alpha)
         "Brighten a COLOR (a hexidecimal string) by a coefficient ALPHA (a float
between 0 and 1)."
         (cond ((and color (symbolp color))
                (doom-lighten (doom-color color) alpha))

               ((listp color)
                (cl-loop for c in color collect (doom-lighten c alpha)))

               ((doom-blend color "#FFFFFF" (- 1 alpha)))))

       (defun doom-color (name &optional type)
         "Retrieve a specific color named NAME (a symbol) from the current theme."
         (let ((colors (if (listp name)
                           name
                         (cdr-safe (assq name doom-themes--colors)))))
           (and colors
                (cond ((listp colors)
                       (let ((i (or (plist-get '(256 1 16 2 8 3) type) 0)))
                         (if (> i (1- (length colors)))
                             (car (last colors))
                           (nth i colors))))
                      (t colors))))))

(defun pb-color_warm (c strength)
  "Make C (hex color string) warmer given STRENGTH ([0..1]) "
  (let ((rgbc (color-name-to-rgb c)))
    (color-rgb-to-hex
     (min 1.0 (+ (car rgbc) strength))
     (cadr rgbc)
     (max 0.0 (- (caddr rgbc) strength))
     2)))

(defun pb-color_cool (c strength)
  "Make C (hex color string) warmer given STRENGTH ([0..1]) "
  (let ((rgbc (color-name-to-rgb c)))
    (color-rgb-to-hex
     (max 0.0 (- (car rgbc) strength))
     (cadr rgbc)
     (min 1.0 (+ (caddr rgbc) strength))
     2)))

(defun iterate-while (f init return)
  (cl-labels ((acc (xs)
                (let* ((x (funcall f (car xs)))
                       (nxt (cons x xs)))
                  (or (funcall return nxt)
                      (acc nxt)))))
    (acc (list init))))

(defun pb-color_derivations (c)
  (let ((iter (lambda (f)
                (iterate-while f c
                               (lambda (xs) (if (equal (car xs) (cadr xs))
                                           (reverse xs))))))
        (comp (pb-color_rgb-to-hex (color-complement c))))
    (list :complement comp
          :triad (pb-color_triad c)
          :warmer (funcall iter (lambda (c) (pb-color_warm c 0.1)))
          :cooler (funcall iter (lambda (c) (pb-color_cool c 0.1)))
          :darker (funcall iter (lambda (c) (doom-darken c 0.1)))
          :cooler (funcall iter (lambda (c) (doom-lighten c 0.1))))))

(defun pb-color_rgb-to-hex (c)
  "Convert the rgb color C to hex string (6 digit)."
  (color-rgb-to-hex (car c) (cadr c) (caddr c) 2))


(defun pb-color_gradient (col1 col2 steps)
  "Produce a sequence of hex colors representing a gradient from COL1 to COL2 in n STEPS."
  (mapcar #'pb-color_rgb-to-hex
         (color-gradient (color-name-to-rgb col1)
                         (color-name-to-rgb col2)
                         steps)))

(defun pb-color_to-hsl (c)
  (apply 'color-rgb-to-hsl
         (color-name-to-rgb c)))

(defun pb-color_from-hsl (c)
  (pb-color_rgb-to-hex (apply 'color-hsl-to-rgb c)))

(defun pb-color_triad (c)
  "Compute color triad based on C (hex string)."
  (let* ((base-color-hsl (pb-color_to-hsl c))
         (base-hue (nth 0 base-color-hsl))
         (saturation (nth 1 base-color-hsl))
         (lightness (nth 2 base-color-hsl)))
    (list c
          (pb-color_from-hsl
           (list (mod (+ base-hue (/ 1.0 3)) 1) saturation lightness))
          (pb-color_from-hsl
           (list (mod (+ base-hue (/ 2.0 3)) 1) saturation lightness)))))

(cl-assert
 (and (equal (pb-color_derivations "#00ff00")
             '(:complement "#ff00ff"
               :triad ("#00ff00" "#0000ff" "#ff0000")
               :warmer ("#00ff00" "#19ff00" "#32ff00" "#4bff00" "#64ff00" "#7dff00" "#96ff00" "#afff00" "#c8ff00" "#e1ff00" "#faff00" "#ffff00" "#ffff00")
               :cooler ("#00ff00" "#00ff19" "#00ff32" "#00ff4b" "#00ff64" "#00ff7d" "#00ff96" "#00ffaf" "#00ffc8" "#00ffe1" "#00fffa" "#00ffff" "#00ffff")
               :darker ("#00ff00" "#00e500" "#00ce00" "#00b900" "#00a600" "#009500" "#008600" "#007800" "#006c00" "#006100" "#005700" "#004e00" "#004600" "#003f00" "#003800" "#003200" "#002d00" "#002800" "#002400" "#002000" "#001c00" "#001900" "#001600" "#001300" "#001100" "#000f00" "#000d00" "#000b00" "#000900" "#000800" "#000700" "#000600" "#000500" "#000400" "#000300" "#000200" "#000100" "#000000" "#000000")
               :cooler ("#00ff00" "#19ff19" "#30ff30" "#44ff44" "#56ff56" "#66ff66" "#75ff75" "#82ff82" "#8eff8e" "#99ff99" "#a3ffa3" "#acffac" "#b4ffb4" "#bbffbb" "#c1ffc1" "#c7ffc7" "#ccffcc" "#d1ffd1" "#d5ffd5" "#d9ffd9" "#dcffdc" "#dfffdf" "#e2ffe2" "#e4ffe4" "#e6ffe6" "#e8ffe8" "#eaffea" "#ecffec" "#edffed" "#eeffee" "#efffef" "#f0fff0" "#f1fff1" "#f2fff2" "#f3fff3" "#f4fff4" "#f5fff5" "#f6fff6" "#f6fff6")))

      (equal (pb-color_triad "#ffaacf")
             '("#ffaacf" "#cfffa9" "#a9ceff"))

      (equal (pb-color_gradient
              "#991322"
              "#291378"
              10)
             '("#8e1329" "#841331" "#7a1339" "#701341" "#661349" "#5b1350" "#511358" "#471360" "#3d1368" "#331370"))))

(provide 'pb-color)
;;; pb-color.el ends here.
