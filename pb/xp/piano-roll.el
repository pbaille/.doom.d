;;; pb/xp/piano-roll.el -*- lexical-binding: t; -*-

(require 'km)
(require 'sq)
(require 'pb-color)
(require 'pb)

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
    (text-scale-set -3)))
