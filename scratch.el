;;; scratch.el -*- lexical-binding: t; -*-



(kurecolor-hex-to-hsv "#000000")

(kurecolor-adjust-brightness
 "#000000"
 0.5)


(setq pb/grey "#7F7F7F")

(setq pb/mid-color (kurecolor-hsv-to-hex 0.5 0.5 0.5))
(setq pb/min-color (kurecolor-hsv-to-hex 0 0 0))
(setq pb/max-color (kurecolor-hsv-to-hex 1 1 1))

(setq pb/colors
      (mapcar (lambda (f)
                (mapcar (lambda (x) (funcall f (kurecolor-hsv-to-hex 0.5 1 1) x))
                        (number-sequence 0 1 0.1)))
              '(kurecolor-hex-set-brightness kurecolor-hex-set-hue kurecolor-hex-set-saturation)))

(kurecolor-hex-get-saturation "#000fff")
(kurecolor-hex-set-saturation "#000fff" 0.5)
(kurecolor-hex-get-saturation "#000fff")

'(autothemer-deftheme example-name "Autothemer example..."

                     ;; Specify the color classes used by the theme
                     ((((class color) (min-colors #xFFFFFF))
                       ((class color) (min-colors #xFF)))

                      ;; Specify the color palette for each of the classes above.
                      (example-red    "#781210" "#FF0000")
                      (example-green  "#22881F" "#00D700")
                      (example-blue   "#212288" "#0000FF")
                      (example-purple "#812FFF" "#Af00FF")
                      (example-yellow "#EFFE00" "#FFFF00")
                      (example-orange "#E06500" "#FF6600")
                      (example-cyan   "#22DDFF" "#00FFFF"))

                     ;; specifications for Emacs faces.
                     ((button (:underline t :weight 'bold :foreground example-red))
                      (error  (:foreground example-orange)))

                     ;; Forms after the face specifications are evaluated.
                     ;; (palette vars can be used, read below for details.)
                     (custom-theme-set-variables 'example-name
                                                 `(ansi-color-names-vector [,example-red
                                                                            ,example-green
                                                                            ,example-blue
                                                                            ,example-purple
                                                                            ,example-yellow
                                                                            ,example-orange
                                                                            ,example-cyan])))
