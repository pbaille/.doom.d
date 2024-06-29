;;; pb/xp/piano-roll.el -*- lexical-binding: t; -*-

(require 'km)
(require 'sq)
(require 'pb-color)
(require 'pb-macros)

(defvar pr-options
  (list :buffer "*piano*"
        :white "gray90"
        :black "gray80"
        :border "gray95"
        :note (pb-color_hsl .2 .7 .5)
        :resolution 80
        :range (cons 48 72)
        :display-scale -3))

(defun pr-block (c &optional length)
  "Build a string of one character of color C."
  (propertize (if length (pb_join-string (sq_repeat length "  ")) "  ")
              'face (list :foreground c :background c)))

(defun pr-line (color &optional len)
  (concat (let ((str (pr-block color (or len (plist-get pr-options :resolution)))))
            (add-face-text-property 0 (length str)
                                    (list :box (list :line-width 1 :color (plist-get pr-options :border)))
                                    'append
                                    str)
            str)
          "\n"))

(defun pr-add-note (y beg end)
  (let ((range (plist-get pr-options :range)))
    (if (and (>= y (car range))
             (<= y (cdr range)))
        (with-current-buffer (get-buffer-create (plist-get pr-options :buffer))
          (let* ((pmin (point-min))
                 (pmax (point-max))
                 (s (buffer-substring pmin pmax))
                 (pixel-width 2)
                 (resolution (* pixel-width (plist-get pr-options :resolution)))
                 (line-length (+ 1 resolution))
                 (n-line (- (cdr range) (car range)))
                 (actual-line (- n-line (- y (car range))))
                 (actual-line-beg (* actual-line line-length))
                 (actual-line-end (+ actual-line-beg line-length))
                 (note-beg (max actual-line-beg
                                (+ actual-line-beg (round (* beg resolution)))))
                 (note-end (min actual-line-end
                                (+ actual-line-beg (round (* end resolution))))))
            (delete-region pmin pmax)
            (add-face-text-property note-beg note-end
                                    (list :background (plist-get pr-options :note))
                                    nil
                                    s)
            (insert s))))))

(defun pr-add-all-keys ()
  (with-current-buffer (get-buffer-create (plist-get pr-options :buffer))
    (let* ((white-line (pr-line (plist-get pr-options :white)))
           (black-line (pr-line (plist-get pr-options :black)))
           (octave (list white-line black-line
                         white-line black-line
                         white-line black-line
                         white-line
                         white-line black-line
                         white-line black-line
                         white-line)))
      (delete-region (point-min) (point-max))
      (insert (pb_join-string (seq-drop (take (cdr (plist-get pr-options :range))
                                              (sq_join (sq_repeat 11 octave)))
                                        (- (car (plist-get pr-options :range))
                                           1)))))
    (text-scale-set (plist-get pr-options :display-scale))))

(progn
  (pr-add-all-keys)
  (pr-add-note 48 0 .5)
  (pr-add-note 60 0 .5)
  (pr-add-note 64 .5 .7)
  (pr-add-note 67 .7 .8)
  (pr-add-note 72 .7 .8)
  (pr-add-note 55 .8 1))
