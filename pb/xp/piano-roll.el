;;; pb/xp/piano-roll.el -*- lexical-binding: t; -*-

(require 'km)
(require 'sq)
(require 'pb-color)
(require 'pb)

(defun pr-block (len color)
  "Build a string of LEN spaces character with background COLOR."
  (propertize (pb_join-string (sq_repeat len " "))
              'face (list :foreground color :background color)))

(defun pr-line (len color border-color)
  (concat (let ((str (pr-block len color)))
            (add-face-text-property 0 (length str)
                                    (list :box (list :line-width 1 :color border-color))
                                    'append
                                    str)
            str)
          "\n"))

(defun pr-note (pitch position duration)
  (km :pitch pitch :position position :duration duration))

(pb_defun pr-add-note [(and (km_keys resolution buffer colors)
                            (km :range (cons pitch-min pitch-max)))
                       (km_keys pitch position duration)]
  (if (and (>= pitch pitch-min)
           (<= pitch pitch-max))
      (with-current-buffer (get-buffer-create buffer)
        (let* ((pmin (point-min))
               (pmax (point-max))
               (s (buffer-substring pmin pmax))
               (line-length (+ 1 resolution))
               (n-line (- pitch-max pitch-min))
               (actual-line (- n-line (- pitch pitch-min)))
               (actual-line-beg (* actual-line line-length))
               (actual-line-end (+ actual-line-beg line-length))
               (note-beg (max actual-line-beg
                              (+ actual-line-beg (round (* position resolution)))))
               (note-end (min actual-line-end
                              (+ actual-line-beg (round (* (+ position duration) resolution))))))
          (delete-region pmin pmax)
          (add-face-text-property note-beg note-end
                                  (list :background (km_get colors :note))
                                  nil
                                  s)
          (insert s)))))

(pb_defun pr-add-all-keys [(km :buffer buf
                               :colors (km_keys white black border)
                               :range (cons pitch-min pitch-max)
                               :display-scale scale
                               :resolution res)]
  (with-current-buffer (get-buffer-create buf)
    (let* ((white-line (pr-line res white border))
           (black-line (pr-line res black border))
           (octave (list white-line black-line
                         white-line black-line
                         white-line black-line
                         white-line
                         white-line black-line
                         white-line black-line
                         white-line)))
      (delete-region (point-min) (point-max))
      (insert (pb-> (sq_join (sq_repeat 11 octave))
                    (sq_take-strict pitch-max)
                    (seq-drop (- pitch-min 1))
                    (pb_join-string))))
    (text-scale-set scale)))

(let ((options (km :buffer "*piano*"
                   :colors (km :white "gray90"
                               :black "gray80"
                               :border "gray95"
                               :note (pb-color_hsl .2 .7 .5))
                   :resolution 200
                   :range (cons 48 72)
                   :display-scale -3))
      (notes (list (pr-note 48 0 .5)
                   (pr-note 60 0 .5)
                   (pr-note 64 .5 .2)
                   (pr-note 67 .7 .1)
                   (pr-note 72 .7 .1)
                   (pr-note 55 .8 .2))))

  (pr-add-all-keys options)
  (dolist (n notes)
    (pr-add-note options n)))
