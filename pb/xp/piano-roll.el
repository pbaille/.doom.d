;;; pb/xp/piano-roll.el -*- lexical-binding: t; -*-

(require 'km)
(require 'sq)
(require 'pb-color)
(require 'pb)

(pb/defun pr-beat-length [(km bars)]
  (seq-reduce (pb/fn [ret (km beat length)]
                     (+ ret (* beat length)))
              bars
              0))

(pb/defun pr-last-note-end-position [(km notes)]
  (seq-reduce (pb/fn [ret (km position duration)]
                     (max ret (+ position duration)))
              notes
              0))

(pb/defun pr-pixel-width [(as pr
                              (km resolution))]
  (* resolution (pr-beat-length pr)))

(pb/defun pr-bar-positions [(km :bars bars)]
  (reverse (seq-reduce (pb/fn [positions (km beat length)]
                              (cons (+ (car positions) (* beat length))
                                    positions))
                       bars
                       (list 0))))

(pb/defun pr-displayable-notes [(as pr
                                    (km :pitch-range (cons pitch-min pitch-max)
                                        :notes notes
                                        :faces faces))]
  (let* ((beat-length (pr-beat-length pr))
         (pixel-width (pr-pixel-width pr))
         (line-length (+ 1 pixel-width)))
    (mapcar (pb/fn [(as note
                        (km position duration pitch))]
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
                             :face (funcall (km/get faces :note)
                                            note
                                            pr)))))
            notes)))

(pb/defun pr-timeline [(as pr (km harmony bars))]
  (let* ((enriched-bars (seq-mapn (pb/fn [idx bar position]
                                         (km/put bar :bar-idx idx :position position))
                                  (sq_range 0 (length bars))
                                  bars
                                  (pr-bar-positions pr)))
         (zones (seq-reduce (pb/fn [(as zones (cons (cons pos last-zone) previous-zones))
                                    (as zone (km type position))]
                                   (let ((nxt-zone (km/merge last-zone zone)))
                                     (if (equal position pos) (cons (cons pos nxt-zone) previous-zones)
                                       (cons (cons position nxt-zone) zones))))
                            (sort (append harmony enriched-bars)
                                  (lambda (a b) (< (km/get a :position) (km/get b :position))))
                            (list (list 0)))))
    (reverse (cons (cons (pr-beat-length pr) ())
                   zones))))

(defun pr-with-timeline (pr)
  "Assoc the computed timeline to PR."
  (km/put pr
          :timeline
          (pr-timeline pr)))

(pb/defun pr-render-grid [(as pr
                              (km timeline pitch-range faces resolution))]
  (let* ((timezones (pb->>
                     timeline
                     (sq_partition 2 1)
                     (mapcar (pb/fn [(list (cons start-pos data) (cons end-pos _))]
                                    (km/put data :start-pos start-pos :end-pos end-pos)))))
         (lines (mapcar (lambda (pitch)
                          (append (seq-mapn (pb/fn [(as data (km start-pos end-pos))]
                                                   (-> (pb/join-string (sq_repeat (* resolution (- end-pos start-pos)) " "))
                                                       (propertize 'face (funcall (km/get faces :grid)
                                                                                  (km/put data :pitch pitch)
                                                                                  pr))))
                                            timezones)
                                  (list "\n")))
                        (reverse (sq_range (car pitch-range)
                                           (+ 1 (cdr pitch-range)))))))
    '(pp (kmq zones enriched-zones))
    (apply #'concat (sq_join lines))))

(pb/defun pr-render [pr]
  (let* ((s (pr-render-grid pr))
         (displayable-notes (pr-displayable-notes pr)))
    (mapc (pb/fn [(km beg end face)]
                 (add-face-text-property beg end face nil s))
          displayable-notes)
    s))

(defvar pr-colors
  (km
   :lines
   (km :light (pb-color :gray90)
       :c-key (pb-color :gray92)
       :dark (pb-color :gray85)
       :border (pb-color :gray95)
       :octave-delimiter (pb-color :gray75))

   :harmonic-functions
   (km :tonic (pb-color :gray73)
       :structural (pb-color :gray80)
       :diatonic (pb-color :gray90)
       :chromatic (pb-color :gray95))

   :channels
   (pb/let [colors (reverse (pb-color/hue-wheel (pb-color/hsl 0 .6 .6) 8))]
       (append colors (mapcar (lambda (c) (pb-color/desaturate c .5))
                              colors)))

   :note-kinds
   (km :tonic (pb-color :azure)
       :structural (pb-color :cyan)
       :diatonic (pb-color :chartreuse (lighten .2) (saturate .2))
       :chromatic (pb-color :orange (lighten .2)))

   :note-default
   (pb-color/hsl .95 .6 .6)))

(setq pr-default-faces
      (km :grid (pb/fn [(km pitch bar-idx bar harmonic-ctx) pr]
                  (pb/let [(km border light dark c-key octave-delimiter) (km/get pr-colors :lines)]
                      (pb->/ (km :box (km :line-width (cons 0 1) :color border))
                             ;; line colors
                             (km/merge _ (if (km/get pr [:options :harmonic-grid])
                                             (pb/let [(km origin struct scale) harmonic-ctx
                                                      m (mod (- pitch (km/get origin :c)) 12)]
                                                 (km :background (km/get pr-colors
                                                                         (list :harmonic-functions
                                                                               (cond ((= 0 m) :tonic)
                                                                                     ((member (sq_index-of scale m) struct) :structural)
                                                                                     ((member m scale) :diatonic)
                                                                                     (t :chromatic))))))
                                           (let* ((pitch-class (mod pitch 12))
                                                  (is-light (member pitch-class (list 0 2 4 5 7 9 11))))
                                             (km :background (if (= 0 pitch-class) c-key (if is-light light dark))
                                                 ;; handles the dark line between B and C (this produces a little shift that I don't like)
                                                 ;; :underline (if (equal 0 (mod pitch 12)) (list :color octave-delimiter :position 0))
                                                 ;; :overline (if (equal 11 (mod pitch 12)) octave-delimiter)
                                                 ))))
                             ;; darken odd bars
                             (if (cl-oddp bar-idx)
                                 (pb-color/walk _ (lambda (c) (pb-color/darken c .03)))
                               _))))

          :note (lambda (note pr)
                  (km :background
                      (pb/if [c (km/get note :color)]
                             (pb-color c)

                             (km/get pr [:options :colors :by-kind])
                             (km/get pr-colors (list :note-kinds (km/get note :kind)))

                             [chan (km/get note :channel)]
                             (nth chan (km/get pr-colors :channels))

                             (km/get pr-colors :note-default))))))

(pb/defun pr-coerce [(as pr
                         (km notes bar))]
  (pr-with-timeline
   (km/upd pr
           :pitch-range (lambda (x) (or x
                                   (pb/let [(cons bottom top)
                                            (seq-reduce (pb/fn [(as bounds
                                                                    (cons pitch-min pitch-max))
                                                                (as note (km pitch))]
                                                               (pb/if (not bounds) (cons pitch pitch)
                                                                      (> pitch pitch-max) (cons pitch-min pitch)
                                                                      (< pitch pitch-min) (cons pitch pitch-max)
                                                                      bounds))
                                                        notes
                                                        nil)
                                            (cons bottom top) (cons (- bottom 2) (+ 2 top))
                                            extra-pad (max 0 (- 12 (- top bottom)))]
                                       (cons (- bottom (floor (/ extra-pad 2.0)))
                                             (+ top (ceiling (/ extra-pad 2.0)))))))
           :bars (lambda (x) (or x
                            (let ((bar (or bar (km :beat 1 :length 4))))
                              (sq_repeat (ceiling (/ (pr-last-note-end-position pr)
                                                     (float (* (km/get bar :beat)
                                                               (km/get bar :length)))))
                                         bar)))))))

(defun pr-make (data)
  (pr-coerce
   (km/merge (km :options (km :harmonic-grid nil)
                 :resolution 32
                 :text-scale -3
                 :faces pr-default-faces)
             data)))

(defun pr-read-plist-from-file (filename)
  (with-temp-buffer
    (insert-file-contents filename)
    (read (current-buffer))))

(defun pr-render-buffer (buf data)
  (with-current-buffer (get-buffer-create buf)
    (erase-buffer)
    (insert (pr-render data))
    (let ((w (or (get-buffer-window (get-buffer buf))
                 (split-window-below))))
      (set-window-buffer w (get-buffer buf))
      (text-scale-set (km/get data :text-scale))
      (window-resize w (- (count-lines (point-min) (point-max))
                          (window-height w))))))

(setq pr-buffers-data ())

(defun pr-get-buffer-data (&optional buf)
  (alist-get (buffer-name (or buf (current-buffer)))
             pr-buffers-data))

(defun pr-upd-buffer-data (buf f)
  (pb/if [buf-name (buffer-name (or buf (current-buffer)))
          data (pr-get-buffer-data buf)
          next-data (funcall f data)]
         (progn (setq pr-buffers-data
                      (cons (cons buf-name next-data)
                            (assq-delete-all buf-name pr-buffers-data)))
                (pr-render-buffer buf-name (km/get next-data :pr-data)))
         (error "Something went wrong with buffer data update")))

(defvar proll-mode-keymap (make-sparse-keymap))

(define-minor-mode proll-mode
  "PRoll"
  :lighter "PRoll"
  :keymap proll-mode-keymap
  (if proll-mode
      (progn (fundamental-mode)
             (setq proll-mode t)
             (evil-normal-state 1)
             (let ((buf (current-buffer)))
               (pb/if ;; [(km content pr-data) (pr-get-buffer-data buf)]
                      ;; (pr-render-buffer buf pr-data)
                      [content (buffer-substring-no-properties (point-min) (point-max))
                       data (pr-make (eval (read content) t))]
                      (progn (setq pr-buffers-data
                                   (cons (cons (buffer-name buf)
                                               (km :content content
                                                   :pr-data data))
                                         pr-buffers-data))
                             (pr-render-buffer buf data)))))
    (progn (erase-buffer)
           (insert (km/get (pr-get-buffer-data (current-buffer))
                           :content))
           (emacs-lisp-mode)
           (setq pr-local-data ()))))

(map! (:map proll-mode-keymap
       :n "h" (lambda () (interactive) (scroll-right 3))
       :n "l" (lambda () (interactive) (scroll-left 3))
       :n "j" (lambda () (interactive) (scroll-up 3))
       :n "k" (lambda () (interactive) (scroll-down 3))
       :n "J" (lambda ()
                (interactive)
                (pr-upd-buffer-data
                 nil (lambda (pr) (km/upd pr [:pr-data :text-scale]
                                     (lambda (s) (- (or s 0) 1))))))
       :n "K" (lambda ()
                (interactive)
                (pr-upd-buffer-data
                 nil (lambda (pr) (km/upd pr [:pr-data :text-scale]
                                     (lambda (s) (+ (or s 0) 1))))))
       :n "H" (lambda ()
                (interactive)
                (pr-upd-buffer-data
                 nil (lambda (pr)
                       (km/upd pr [:pr-data :resolution]
                               (lambda (r) (- (or r 32) 4))))))
       :n "L" (lambda ()
                (interactive)
                (pr-upd-buffer-data
                 nil (lambda (pr)
                       (km/upd pr [:pr-data :resolution]
                               (lambda (r) (+ (or r 32) 4))))))))

(quote
 (list
  (setq pr-buffers-data ())
  (pr-with-buf
   (erase-buffer)
   (insert (format "'%s" (pr-read-plist-from-file "~/Code/WIP/noon/src/noon/doc/sample-pr.el")))
   (proll-mode 1))))

(quote
 (list

  (defvar pr-buf (get-buffer-create "*pr*"))

  (defmacro pr-with-buf (&rest body)
    `(with-current-buffer (get-buffer-create "*pr*")
       ,@body))


  (defun pr-update-timerange-face (buf start end f)
    (pb/let [(km pr-data) (pr-get-buffer-data buf)
             (cons pitch-min pitch-max) (km/get pr-data :pitch-range)
             width (pr-pixel-width pr-data)
             beat-length (pr-beat-length pr-data)]
        (mapc (lambda (i)
                (let* ((offset (* i (+ 1 width)))
                       (start-pos (round (* width (/ start (float beat-length)))))
                       (end-pos (round (* width (/ end (float beat-length))))))
                  (pb-text-properties_update-faces
                   buf
                   (+ 1 start-pos offset)
                   (+ 1 end-pos offset)
                   (lambda (face) (funcall f (- pitch-max i) face pr-data)))))
              (sq_range 0 (+ 1 (- pitch-max pitch-min))))))

  (pr-update-timerange-face (get-buffer "*pr*")
                            2 3
                            (lambda (pitch face pr)
                                (km/upd face :background
                                      (lambda (c) (pb-color c (saturate .5) (rotate (/ pitch (float 10))))))))

  (pr-with-buf
   (pb/let [(km pr-data) (pr-get-buffer-data)]
       (mapc (pb/fn [(and (km position duration)
                          (km :harmonic-ctx (km origin struct scale)))]
                    (pr-update-timerange-face pr-buf
                                              position (+ position duration)
                                              (lambda (pitch face pr)
                                                (let ((m (mod (- pitch (km/get origin :c)) 12)))
                                                  (km/upd face :background
                                                          (lambda (c) (pb-color c (blend (km/get pr-colors (list :harmonic-functions
                                                                                                      (cond ((= 0 m) :tonic)
                                                                                                            ((member (-elem-index m scale) struct) :structural)
                                                                                                            ((member m scale) :diatonic)
                                                                                                            (t :chromatic))))
                                                                                    .7))))))))
             (km/get pr-data :harmony))))

       ))

(quote
 (list :tests
  (defvar pr-sample-data
    (pr-make (km :bars (list (km :beat 1 :length 4)
                             (km :beat 1 :length 2)
                             (km :beat 1 :length 3)
                             (km :beat 1 :length 3))
                 :notes (list (km :pitch 60 :position 0 :duration 2)
                              (km :pitch 64 :position 2 :duration 4)
                              (km :pitch 67 :position 6 :duration 1)
                              (km :pitch 72 :position 7 :duration 1)
                              (km :pitch 69 :position 8 :duration 3)
                              (km :pitch 66 :position 8 :duration 3)))))

  (pr-render-buffer "*pr*"
                    (pr-make (pr-read-plist-from-file "~/Code/WIP/noon/src/noon/doc/sample-pr.el")))

  (pr-render-buffer "*pr*"
                    (pr-make (km :notes (list (km :pitch 60 :position 0 :duration 2)
                                              (km :pitch 64 :position 2 :duration 4)
                                              (km :pitch 67 :position 6 :duration 1)
                                              (km :pitch 72 :position 7 :duration 1)
                                              (km :pitch 69 :position 8 :duration 3)
                                              (km :pitch 66 :position 8 :duration 3)))))

  (pb-text-properties_update-faces (get-buffer-create "*pr*")
                                   100 200
                                   (lambda (pl) (pb-color/walk pl (pb-color/f> (blend "red" .5)))))))
