;;; pb/xp/piano-roll.el -*- lexical-binding: t; -*-

(require 'km)
(require 'sq)
(require 'pb-color)
(require 'pb)

(quote
 (list :deprecated
  (defun pr--extend-face-properties (s props &optional from to)
    (let ((s2 (copy-sequence s)))
      (add-face-text-property (or from 0) (or to (length s2)) props nil s2)
      s2))

  (defun pr--block (len color)
    "Build a string of LEN spaces character with background COLOR."
    (propertize (pb_join-string (sq_repeat len " "))
                'face (list :foreground color :background color)))

  (defun pr--line (len color border-color)
    (pb-> (pr--block len color)
          (pr--extend-face-properties (list :box (list :line-width 1 :color border-color)))
          (concat "\n")))

  (defun pr-note (pitch position duration &optional face)
    (km :pitch pitch :position position :duration duration :face face))

  (pb_defun pr-initial [(and options
                             (km :colors (km_keys white black border)
                                 :range (cons pitch-min pitch-max)
                                 :resolution res))]
    (let* ((white-line (pr--line res white border))
           (black-line (pr--line res black border))
           (octave (list white-line black-line
                         white-line black-line
                         white-line black-line
                         white-line
                         white-line black-line
                         white-line black-line
                         white-line)))
      (km_put options
              :string (pb-> (sq_join (sq_repeat 11 octave))
                            (sq_take-strict pitch-max)
                            (seq-drop (- pitch-min 1))
                            (pb_join-string)))))

  (pb_defun pr-add-note [(and piano-roll
                              (km :resolution res
                                  :range (cons pitch-min pitch-max)))
                         (km_keys pitch position duration face)]
    (if (and (>= pitch pitch-min)
             (<= pitch pitch-max))
        (let* ((line-length (+ 1 res))
               (n-line (- pitch-max pitch-min))
               (actual-line (- n-line (- pitch pitch-min)))
               (actual-line-beg (* actual-line line-length))
               (actual-line-end (+ actual-line-beg line-length))
               (note-beg (max actual-line-beg
                              (+ actual-line-beg (round (* position res)))))
               (note-end (min actual-line-end
                              (+ actual-line-beg (round (* (+ position duration) res))))))
          (km_upd piano-roll
                  :string (lambda (prs)
                            (pr--extend-face-properties prs
                                                        face
                                                        note-beg
                                                        note-end))))
      piano-roll))

  (let* ((options (km :colors (km :white "gray90"
                                  :black "gray80"
                                  :border "gray95")
                      :resolution 200
                      :range (cons 48 72)))
         (note-face (list :background (pb-color_hsl .2 .7 .5)))
         (notes (list (pr-note 48 0 .5 note-face)
                      (pr-note 60 0 .5 note-face)
                      (pr-note 64 .5 .2 note-face)
                      (pr-note 67 .7 .1 note-face)
                      (pr-note 72 .7 .1 note-face)
                      (pr-note 55 .8 .2 note-face)))
         (pr (seq-reduce (lambda (pr n)
                           (pr-add-note pr n))
                         notes
                         (pr-initial options))))
    (with-current-buffer (get-buffer-create "*piano*")
      (delete-region (point-min) (point-max))
      (insert (km_get pr :string))
      ))))

(defvar pr-new
  (km :pitch-range (cons 58 74)
      :resolution 32
      :bars (list (km :beat 1 :length 4)
                  (km :beat 1 :length 4))
      :notes (list (km :pitch 60 :position 0 :duration 2)
                   (km :pitch 64 :position 2 :duration 4)
                   (km :pitch 67 :position 6 :duration 1)
                   (km :pitch 72 :position 7 :duration 1))
      :faces (km :line (lambda (pitch)
                         (let ((pitch-class (mod pitch 12)))
                           (if (member pitch-class (list 0 2 4 5 7 9 11))
                               (km :background "gray90" :box (km :line-width (cons 0 1) :color "gray95"))
                             (km :background "gray80" :box (km :line-width (cons 0 1) :color "gray95")))))
                 :note (lambda (note) (km :background (pb-color_hsl .2 .7 .5)))
                 :bar (lambda (bar)
                        (km :box (km :line-width (cons 1 1) :color "gray95"))))))

(pb_defun pr-beat-length [(km_keys bars)]
  (seq-reduce (pb_fn [ret (km_keys beat length)]
                     (+ ret (* beat length)))
              bars
              0))

(pb_defun pr-pixel-width [(and pr
                               (km_keys resolution))]
  (* resolution (pr-beat-length pr)))

(pb_defun pr-bar-positions [(km :bars bars)]
  (reverse (seq-reduce (pb_fn [positions (km_keys beat length)]
                              (cons (+ (car positions) (* beat length))
                                    positions))
                       bars
                       (list 0))))

(pb_defun pr-displayable-notes [(and pr
                                     (km :pitch-range (cons pitch-min pitch-max)
                                         :notes notes
                                         :faces faces))]
  (let* ((beat-length (pr-beat-length pr))
         (pixel-width (pr-pixel-width pr))
         (line-length (+ 1 pixel-width)))
    (mapcar (pb_fn [(and note
                         (km_keys position duration pitch))]
                   (if (and (>= pitch pitch-min)
                            (<= pitch pitch-max))
                       (let* ((n-line (- pitch-max pitch-min))
                              (actual-line (- n-line (- pitch pitch-min)))
                              (actual-line-beg (* actual-line line-length))
                              (actual-line-end (+ actual-line-beg line-length))
                              (note-beg (max actual-line-beg
                                             (+ actual-line-beg (round (* pixel-width (/ position (float beat-length)))))))
                              (note-end (min actual-line-end
                                             (+ actual-line-beg (round (* pixel-width (/ (+ position duration) (float beat-length))))))))
                         (km :beg note-beg
                             :end note-end
                             :face (funcall (km_get faces :note)
                                            note)))))
            notes)))

(pb_defun pr-render-grid [(and pr (km_keys pitch-range faces))]
  (let* ((pixel-width (pr-pixel-width pr))
         (bar-positions (pr-bar-positions pr))
         (lines (mapcar (lambda (pitch)
                          (propertize (pb_join-string (sq_repeat pixel-width " "))
                                      'face (funcall (km_get faces :line)
                                                     pitch)))
                        (sq_range (car pitch-range) (+ 1 (cdr pitch-range)))))
         (s (pb_join-string (reverse lines) "\n")))
    s))

(pb_defun pr-render [pr]
  (let* ((s (pr-render-grid pr))
         (displayable-notes (pr-displayable-notes pr)))
    (mapc (pb_fn [(km_keys beg end face)]
                 (add-face-text-property beg end face nil s))
          displayable-notes)
    s))

(pr-displayable-notes pr-new)
(pr-displayable-bars pr-new)


(with-current-buffer (get-buffer-create "*pr*")
  (delete-region (point-min) (point-max))
  (insert (pr-render pr-new))
  (text-scale-set -3))

(quote
 (list :mess
  (pb_defun pr-displayable-bars [(and pr
                                      (km_keys bars pitch-range faces))]
    (let* ((beat-length (pr-beat-length pr))
           (pixel-width (pr-pixel-width pr))
           (line-length (+ 1 pixel-width))
           (line-count (- (cdr pitch-range)
                          (car pitch-range)))
           (bar-positions (seq-reduce (pb_fn [positions (km_keys beat length)]
                                             (cons (+ (car positions) (* beat length))
                                                   positions))
                                      bars
                                      (list 0)))
           (bar-faces (mapcar (lambda (bar) (funcall (km_get faces :bar)
                                                bar))
                              bars)))
      (seq-reduce (pb_fn [ret (cons (list beg-pos end-pos) face)]
                         (append ret
                                 (mapcar (lambda (line-idx)
                                           (let* ((line-beg (* line-idx line-length))
                                                  (line-end (+ line-beg line-length))
                                                  (bar-beg (max line-beg
                                                                (+ line-beg (round (* pixel-width (/ beg-pos (float beat-length)))))))
                                                  (bar-end (min line-end
                                                                (+ line-beg (round (* pixel-width (/ end-pos (float beat-length))))))))
                                             (km :beg bar-beg
                                                 :end bar-end
                                                 :face face)))
                                         (sq_range 0 line-count))))
                  (seq-mapn #'cons
                            (sq_partition 2 1 (reverse bar-positions))
                            bar-faces)
                  ())))
  (pb_defun pr-bar-positions [(km :bars bars)]
    (reverse (seq-reduce (pb_fn [positions (km_keys beat length)]
                                (cons (+ (car positions) (* beat length))
                                      positions))
                         bars
                         (list 0))))

  (pb_defun pr-render [(and pr (km_keys pitch-range faces))]
    (let* ((pixel-width (pr-pixel-width pr))
           (bar-positions (pr-bar-positions pr))
           (lines (mapcar (lambda (pitch)
                            (apply #'concat
                                   (mapcar (pb_fn [(cons beg end)]
                                                  (propertize (pb_join-string (sq_repeat (* resolution (- end beg)) " "))
                                                              'face (funcall (km_get faces :line)
                                                                             pitch)))
                                           (sq_partition 2 1 bar-positions))))
                          (sq_range (car pitch-range) (+ 1 (cdr pitch-range)))))
           (s (pb_join-string (reverse lines) "\n"))
           (displayable-notes (pr-displayable-notes pr)))
      (mapc (pb_fn [(km_keys beg end face)]
                   (add-face-text-property beg end face nil s))
            displayable-notes)
      s))))
