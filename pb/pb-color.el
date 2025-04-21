;;; pb-color.el --- hex colors utility -*- lexical-binding: t; -*-

;; Author: Pierre Baille
;; URL: https://github.com/pbaille
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; A utility package for dealing with colors as hex strings.

;;; Code:

(require 'color)
(require 'cl-lib)
(require 'sq)

(defvar pb-color/6-hue-names
  '(:red :yellow :green :cyan :blue :magenta))

(defvar pb-color/12-hue-names
  '(:red :orange :yellow :chartreuse :green :spring :cyan :azure :blue :violet :magenta :rose))

(defun pb-color/hex-color-p (x)
  "Check if X is an hex color string."
  (and (stringp x)
       (string-prefix-p "#" x)))

(defun pb-color/hsl-p (x)
  "Check if X is a valid HSL list."
  (and (consp x)
       (= 3 (length x))
       (cl-every (lambda (x)
                   (and (numberp x)
                        (<= 0 x 1)))
                 x)))

(defun pb-color/from-name (name)
  "Produce an hex string from a color NAME."
  (let ((name (cond ((stringp name) name)
                    ((keywordp name) (substring (symbol-name name) 1))
                    ((symbolp name) (symbol-name name)))))
    (if-let ((rgb (color-name-to-rgb name)))
        (pb-color/from-rgb rgb)
      (error (format "Unknown color: %s" name)))))

(defun pb-color/from-rgb (c)
  "Convert the rgb color C to hex string (6 digit)."
  (cl-destructuring-bind (r g b) c
    (color-rgb-to-hex r g b 2)))

(defun pb-color/to-hsl (c)
  "Convert C from hex to hsl."
  (cl-destructuring-bind (r g b) (color-name-to-rgb c)
    (color-rgb-to-hsl r g b)))

(defun pb-color/from-hsl (c)
  "Convert C from hsl to hex."
  (cl-destructuring-bind (h s l) c
    (pb-color/from-rgb (color-hsl-to-rgb h s l))))

(defun pb-color/hsl (hue saturation lightness)
  "Create a color based on HUE, SATURATION and LIGHTNESS."
  (pb-color/from-hsl (list hue saturation lightness)))

(defun pb-color/random-value ()
  "Return a random value between 0 and 1."
  (/ (random 255) 255.0))

(defun pb-color/random ()
  "Create a random color."
  (pb-color/from-rgb (list (pb-color/random-value)
                           (pb-color/random-value)
                           (pb-color/random-value))))

(defun pb-color/blend (c1 c2 alpha)
  "Blend C1 with C2 by a coefficient ALPHA (a float between 0 and 1)."
  (apply (lambda (r g b) (format "#%02x%02x%02x" (* r 255) (* g 255) (* b 255)))
         (cl-loop for it    in (color-name-to-rgb c1)
                  for other in (color-name-to-rgb c2)
                  collect (+ (* alpha it) (* other (- 1 alpha))))))

(defun pb-color/darken (c alpha)
  "Darken C by a coefficient ALPHA (a float between 0 and 1)."
  (pb-color/blend c "#000000" (- 1 alpha)))

(defun pb-color/lighten (c alpha)
  "Brighten a C by a coefficient ALPHA (a float between 0 and 1)."
  (pb-color/blend c "#FFFFFF" (- 1 alpha)))

(defun pb-color/warm (c alpha)
  "Make C warmer by a coefficient ALPHA (a float between 0 and 1)."
  (cl-destructuring-bind (r g b) (color-name-to-rgb c)
    (color-rgb-to-hex
     (+ r (* (- 1 r) alpha))
     (+ g (* (- 0.6 g) alpha)) ; modified here
     (- b (* b alpha)) ; and here
     2)))

(defun pb-color/cool (c alpha)
  "Make C cooler by a coefficient ALPHA (a float between 0 and 1)."
  (cl-destructuring-bind (r g b) (color-name-to-rgb c)
    (color-rgb-to-hex
     (- r (* r alpha))
     (- g (* g alpha))
     (+ b (* (- 1 b) alpha))
     2)))

(defun pb-color/warmings (c n)
  "Warm C in N step."
  (cl-loop for x from 1 to n by 1
           collect (pb-color/warm c (* x (/ 1.0 n)))))

(defun pb-color/coolings (c n)
  "Cool C in N step."
  (cl-loop for x from 1 to n by 1
           collect (pb-color/cool c (* x (/ 1.0 n)))))

(defun pb-color/complementary (c)
  "Compute the complementary color of C."
  (pb-color/from-rgb (color-complement c)))

(defun pb-color/analogous (c res)
  "Compute two analogous colors based on C.
RES is resolution."
  (let ((hue-wheel (pb-color/hue-wheel c res)))
    (list (cadr hue-wheel)
          (car (reverse hue-wheel)))))

(defun pb-color/split-complementary (c resolution)
  "Compute the C split complementary colors based on RESOLUTION."
  (pb-color/analogous (pb-color/complementary c)
                      resolution))


(defun pb-color/gradient (col1 col2 n)
  "Produce a gradient (list of colors) from COL1 to COL2 in N step."
  (mapcar #'pb-color/from-rgb
          (color-gradient (color-name-to-rgb col1)
                          (color-name-to-rgb col2)
                          n)))

(defun pb-color/triad (c)
  "Compute color triad based on C (hex string)."
  (cl-destructuring-bind (h s l) (pb-color/to-hsl c)
    (list c
          (pb-color/from-hsl
           (list (mod (+ h (/ 1.0 3)) 1) s l))
          (pb-color/from-hsl
           (list (mod (+ h (/ 2.0 3)) 1) s l)))))

(defun pb-color/tetrad (c)
  "Compute color tetrad based on C (hex string)."
  (cl-destructuring-bind (h s l) (pb-color/to-hsl c)
    (list c
          (pb-color/from-hsl
           (list (mod (+ h (/ 1.0 4)) 1) s l))
          (pb-color/from-hsl
           (list (mod (+ h (/ 1.0 2)) 1) s l))
          (pb-color/from-hsl
           (list (mod (+ h (/ 3.0 4)) 1) s l)))))

(defun pb-color/hue-wheel (c n)
  "Compute hue wheel of size N starting on C."
  (cl-destructuring-bind (h s l) (pb-color/to-hsl c)
    (cl-loop for x from 0 to (- n 1) by 1
             collect (pb-color/from-hsl
                      (list (mod (+ h (/ x (float n))) 1) s l)))))

(defun pb-color/saturations (c n)
  "Compute N gradual saturations based on C."
  (cl-destructuring-bind (h s l) (pb-color/to-hsl c)
    (let ((increment (/ (- 1 s) (float n))))
      (cl-loop for x from 1 to n by 1
               collect (pb-color/from-hsl (list h (+ s (* x increment)) l))))))

(defun pb-color/desaturations (c n)
  "Compute N gradual desaturations based on C."
  (cl-destructuring-bind (h s l) (pb-color/to-hsl c)
    (let ((increment (/ s (float n))))
      (cl-loop for x from 1 to n by 1
               collect (pb-color/from-hsl (list h (- s (* x increment)) l))))))

(defun pb-color/rotate (c ratio)
  "Rotate C hue by RATIO."
  (cl-destructuring-bind (h s l) (pb-color/to-hsl c)
    (pb-color/from-hsl (list (mod (+ h ratio) 1) s l))))

(defun pb-color/saturate (c alpha)
  "Saturate C by ALPHA."
  (cl-destructuring-bind (h s l) (pb-color/to-hsl c)
    (pb-color/from-hsl (list h (+ s (* alpha (- 1 s))) l))))

(defun pb-color/desaturate (c alpha)
  "Desaturate C by ALPHA."
  (cl-destructuring-bind (h s l) (pb-color/to-hsl c)
    (pb-color/from-hsl (list h (- s (* alpha s)) l))))

(defun pb-color/lights (c n)
  "Compute N gradualy lighter shades of C."
  (cl-destructuring-bind (h s l) (pb-color/to-hsl c)
    (let ((increment (/ (- 1 l) (float n))))
      (cl-loop for x from 1 to n by 1
               collect (pb-color/from-hsl (list h s (+ l (* x increment))))))))

(defun pb-color/shades (c n)
  "Compute N gradualy darker shades of C."
  (cl-destructuring-bind (h s l) (pb-color/to-hsl c)
    (let ((increment (/ l (float n))))
      (cl-loop for x from 1 to n by 1
               collect (pb-color/from-hsl (list h s (- l (* x increment))))))))


(defun pb-color/hue (c)
  "Get the hue of C."
  (nth 0 (pb-color/to-hsl c)))

(defun pb-color/saturation (c)
  "Get the saturation of C."
  (nth 1 (pb-color/to-hsl c)))

(defun pb-color/lightness (c)
  "Get the lightness of C."
  (nth 2 (pb-color/to-hsl c)))

(defun pb-color/set-hue (c hue)
  "Change the HUE of C."
  (cl-destructuring-bind (_ s l) (pb-color/to-hsl c)
    (pb-color/from-hsl (list (mod hue 1) s l))))

(defun pb-color/set-saturation (c saturation)
  "Change the SATURATION of C."
  (cl-destructuring-bind (h _ l) (pb-color/to-hsl c)
    (pb-color/from-hsl (list h (min 1 (max 0 saturation)) l))))

(defun pb-color/set-lightness (c lightness)
  "Change the LIGHTNESS of C."
  (cl-destructuring-bind (h s _) (pb-color/to-hsl c)
    (pb-color/from-hsl (list h s (min 1 (max 0 lightness))))))

(defun pb-color/update-hue (c f)
  "Update the hue of C using F."
  (pb-color/set-hue c (funcall f (pb-color/hue c))))

(defun pb-color/update-saturation (c f)
  "Update the saturation of C using F."
  (pb-color/set-saturation c (funcall f (pb-color/saturation c))))

(defun pb-color/update-lightness (c f)
  "Update the lightness of C using F."
  (pb-color/set-lightness c (funcall f (pb-color/lightness c))))

(defun pb-color/complementary-hue (c)
  "Change the hue component of C to its complementary value."
  (pb-color/update-hue
   c (lambda (h) (mod (+ h .5) 1))))

(defun pb-color/complementary-saturation (c)
  "Change the saturation component of C to its complementary value."
  (pb-color/update-saturation
   c (lambda (s) (mod (+ s .5) 1))))

(defun pb-color/complementary-lightness (c)
  "Change the lightness component of C to its complementary value."
  (pb-color/update-lightness
   c (lambda (l) (mod (+ l .5) 1))))

(defun pb-color/symetric-lightness (c)
  "Change the lightness component of C to its symetric value (around 0.5)."
  (pb-color/update-lightness
   c (lambda (l) (if (> l 0.5)
                (- .5 (- l .5))
              (+ .5 (- .5 l))))))

(defun pb-color/symetric-saturation (c)
  "Change the saturation component of C to its symetric value (around 0.5)."
  (pb-color/update-saturation
   c (lambda (s) (if (> s 0.5)
                (- .5 (- s .5))
              (+ .5 (- .5 s))))))

(defun pb-color/neutralize-saturation (c ratio)
  "Bring the saturation of C toward its middle value 0.5.
if RATIO is 1 the saturation will be 0.5, if it is 0 it will be unchanged."
  (pb-color/update-saturation
   c (lambda (s) (+ s (* ratio (- .5 s))))))

(defun pb-color/neutralize-lightness (c ratio)
  "Bring the lightness of C toward its middle value 0.5.
if RATIO is 1 the lightness will be 0.5, if it is 0 it will be unchanged."
  (pb-color/update-lightness
   c (lambda (l) (+ l (* ratio (- .5 l))))))

(defun pb-color/exacerbate-saturation (c ratio)
  "Push the saturation of C away from its middle value 0.5.
if RATIO is 1 the saturation will be 0 or 1 depending on C, if it is 0 it will be unchanged."
  (pb-color/update-saturation
   c (lambda (s) (+ s (* ratio (if (> s 0.5)
                              (- 1 s)
                            (- s)))))))

(defun pb-color/exacerbate-lightness (c ratio)
  "Push the lightness of C away from its middle value 0.5.
if RATIO is 1 the lightness will be 0 or 1 depending on C, if it is 0 it will be unchanged."
  (pb-color/update-lightness
   c (lambda (l) (+ l (* ratio (if (> l 0.5)
                              (- 1 l)
                            (- l)))))))

(defun pb-color/luminance (c)
  "Compute the luminance of C."
  (cl-destructuring-bind (r g b) (color-name-to-rgb c)
    (+ (* 0.2126 r) (* 0.7152 g) (* 0.0722 b))))

(defun pb-color/contrast (c1 c2)
  "Compute the contrast ratio between C1 and C2."
  (let* ((lum1 (pb-color/luminance c1))
         (lum2 (pb-color/luminance c2))
         (l1 (max lum1 lum2))
         (l2 (min lum1 lum2)))
    (/ (- (/ (+ l1 0.05)
             (+ l2 0.05))
          1)
       20.0)))

(defun pb-color/walk (data f)
  "Walk some DATA, applying F to every nested colors (hex strings)."
  (cond ((consp data)
         (cons (pb-color/walk (car data) f)
               (pb-color/walk (cdr data) f)))
        ((pb-color/hex-color-p data)
         (funcall f data))
        (t data)))

(defun pb-color/overview (c res)
  "Compute some other colors related to C using functino from the pb-color package.

RES is the resolution to be used for computations."
  (list :complement (pb-color/complementary c)
        :analogous (pb-color/analogous c res)
        :split-complementary (pb-color/split-complementary c res)
        :triad (pb-color/triad c)
        :tetrad (pb-color/tetrad c)
        :warmings (pb-color/warmings c res)
        :coolings (pb-color/coolings c res)
        :shades (pb-color/shades c res)
        :lights (pb-color/lights c res)
        :saturations (pb-color/saturations c res)
        :desaturations (pb-color/desaturations c res)))


(defvar pb-color/user-palette
  (sq_interleave pb-color/12-hue-names
                 (pb-color/hue-wheel (pb-color/hsl 0 .5 .5)
                                     12)))

(defun pb-color/coerce (c)
  "Coerce C to an hex color string."
  (or (plist-get pb-color/user-palette c)
      (cond ((pb-color/hex-color-p c) c)
            ((pb-color/hsl-p c) (pb-color/from-hsl c))
            ((or (stringp c)
                 (symbolp c))
             (pb-color/from-name c)))
      (error (format "Unknown color: %s" c))))

(defmacro pb-color (c &rest transformations)
  "Thread C through TRANSFORMATIONS prefixing them with pb-color."
  (seq-reduce (lambda (ret form)
                `(,(intern (concat "pb-color/" (symbol-name (car form))))
                  ,ret
                  ,@(cdr form)))
              transformations
              (if (or (stringp c) (keywordp c))
                  (pb-color/coerce c)
                (list 'pb-color/coerce c))))

(defmacro pb-color/f> (&rest transformations)
  "Thread C through TRANSFORMATIONS prefixing them with pb-color."
  `(lambda (c) (pb-color c ,@transformations)))

(defun pb-color/test ()
  (cl-assert
   (and (equal (pb-color/triad "#ffaacf")
               '("#ffaacf" "#cfffa9" "#a9ceff"))

        (equal (pb-color/tetrad "#ffaacf")
               '("#ffaacf" "#f9ffa9" "#a9ffda" "#afa9ff"))

        (equal (pb-color/gradient
                "#991322"
                "#291378"
                10)
               '("#8e1329" "#841331" "#7a1339" "#701341" "#661349" "#5b1350" "#511358" "#471360" "#3d1368" "#331370"))

        (equal
         (pb-color/contrast "black" "white")
         (pb-color/contrast "white" "black"))

        (equal
         1.0
         (pb-color/contrast "white" "black"))

        (equal
         0.0
         (pb-color/contrast "white" "white"))

        (equal .0 (pb-color/luminance "black"))
        (equal 1.0 (pb-color/luminance "white")))))

(provide 'pb-color)
;;; pb-color.el ends here.
