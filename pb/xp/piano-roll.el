;;; pb/xp/piano-roll.el -*- lexical-binding: t; -*-

(require 'km)
(require 'sq)
(require 'pb-color)
(require 'pb)

(pb_defun pr-beat-length [(km_keys bars)]
  (seq-reduce (pb_fn [ret (km_keys beat length)]
                     (+ ret (* beat length)))
              bars
              0))

(pb_defun pr-last-note-end-position [(km_keys notes)]
  (seq-reduce (pb_fn [ret (km_keys position duration)]
                     (max ret (+ position duration)))
              notes
              0))

(pb_defun pr-pixel-width [(as pr
                              (km_keys resolution))]
  (* resolution (pr-beat-length pr)))

(pb_defun pr-bar-positions [(km :bars bars)]
  (reverse (seq-reduce (pb_fn [positions (km_keys beat length)]
                              (cons (+ (car positions) (* beat length))
                                    positions))
                       bars
                       (list 0))))

(pb_defun pr-displayable-notes [(as pr
                                    (km :pitch-range (cons pitch-min pitch-max)
                                        :notes notes
                                        :faces faces))]
  (let* ((beat-length (pr-beat-length pr))
         (pixel-width (pr-pixel-width pr))
         (line-length (+ 1 pixel-width)))
    (mapcar (pb_fn [(as note
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

(pb_defun pr-render-grid [(as pr
                              (km_keys bars pitch-range faces resolution))]
  (let* ((bar-positions (pr-bar-positions pr))
         (lines (mapcar (lambda (pitch)
                          (append (seq-mapn (pb_fn [i bar (list beg end)]
                                                   (-> (pb_join-string (sq_repeat (* resolution (- end beg)) " "))
                                                       (propertize 'face (funcall (km_get faces :bar)
                                                                                  (km_put bar :idx i)
                                                                                  (funcall (km_get faces :line)
                                                                                           pitch)))))
                                            (sq_range 0 (length bars))
                                            bars
                                            (sq_partition 2 1 bar-positions))
                                  (list "\n")))
                        (reverse (sq_range (car pitch-range)
                                           (+ 1 (cdr pitch-range)))))))
    (apply #'concat (sq_join lines))))

(pb_defun pr-render [pr]
  (let* ((s (pr-render-grid pr))
         (displayable-notes (pr-displayable-notes pr)))
    (mapc (pb_fn [(km_keys beg end face)]
                 (add-face-text-property beg end face nil s))
          displayable-notes)
    s))

(defvar pr-default-faces
  (let* (;; colors
         (bg-light "gray90")
         (bg-dark "gray80")
         (bg-note (pb-color_hsl .95 .6 .6))
         (grid-border-color "gray95"))
    (km :line (lambda (pitch)
                (let* ((is-light (member (mod pitch 12) (list 0 2 4 5 7 9 11))))
                  (km :box (km :line-width (cons 0 1) :color grid-border-color)
                      :background (if is-light bg-light bg-dark))))
        :note (lambda (note)
                (let ((color (or (km_get note :color)
                                 (pcase (km_get note :kind)
                                   (:tonic (pb-color :azure))
                                   (:structural (pb-color :cyan))
                                   (:diatonic (pb-color :chartreuse (lighten .2) (saturate .2)))
                                   (:chromatic (pb-color :orange (lighten .2)))
                                   (_ bg-note)))))
                  (km :background color)))
        :bar (lambda (bar face)
               (km_upd face :background
                       (lambda (bg)
                         (if (cl-oddp (km_get bar :idx))
                             (pb-color_darken bg .05)
                           bg)))))))

(pb_defun pr-coerce [(as pr
                         (km_keys notes bar))]
  (km_upd pr
          :pitch-range (lambda (x) (or x
                                  (pb_let [(cons min max)
                                           (seq-reduce (pb_fn [(as bounds
                                                                   (cons pitch-min pitch-max))
                                                               (as note (km_keys pitch))]
                                                              (pb_if (not bounds) (cons pitch pitch)
                                                                     (> pitch pitch-max) (cons pitch-min pitch)
                                                                     (< pitch pitch-min) (cons pitch pitch-max)
                                                                     bounds))
                                                       notes
                                                       nil)]
                                      (cons (- min 3) (+ 3 max)))))
          :bars (lambda (x) (or x
                           (let ((bar (or bar (km :beat 1 :length 4))))
                             (sq_repeat (ceiling (/ (pr-last-note-end-position pr)
                                                    (float (* (km_get bar :beat)
                                                              (km_get bar :length)))))
                                        bar))))))

(defun pr-make (data)
  (pr-coerce
   (km_merge (km :resolution 32
                 :faces pr-default-faces)
             data)))

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

(defun pr-read-plist-from-file (filename)
  (with-temp-buffer
    (insert-file-contents filename)
    (read (current-buffer))))

(defun pr-render-buffer (buf data)
  (print (km :pr-render-buffer (kmq buf data)))
  (with-current-buffer (get-buffer-create buf)
    (erase-buffer)
    (insert (pr-render data))
    (let ((w (or (get-buffer-window (get-buffer buf))
                 (split-window-below))))
      (set-window-buffer w (get-buffer buf))
      (window-resize w (- (count-lines (point-min) (point-max))
                          (window-height w))))))

(quote
 (pr-render-buffer "*pr*"
  (pr-read-plist-from-file "~/Code/WIP/noon/src/noon/doc/sample-pr.el")))

(quote
 (pr-render-buffer "*pr*"
  pr-sample-data))

(defvar pr-buffers-data ())
(setq pr-buffers-data ())

(defun pr-get-buffer-data (&optional buf)
  (alist-get (buffer-name (or buf (current-buffer)))
             pr-buffers-data))

(defun pr-upd-buffer-data (buf f)
  (pb_if [buf-name (buffer-name (or buf (current-buffer)))
          data (pr-get-buffer-data buf)
          next-data (funcall f data)]
         (progn (setq pr-buffers-data
                      (cons (cons buf-name next-data)
                            (assq-delete-all buf-name pr-buffers-data)))
                (pr-render-buffer buf-name (km_get next-data :pr-data))
                (text-scale-set (km_get next-data :text-scale)))
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
               (pb_if [(km_keys content pr-data) (pr-get-buffer-data buf)]
                      (pr-render-buffer buf pr-data)
                      [content (buffer-substring-no-properties (point-min) (point-max))
                               data (pr-make (eval (read content) t))]
                      (progn (setq pr-buffers-data
                                   (cons (cons (buffer-name buf)
                                               (km :content content
                                                   :pr-data data
                                                   :text-scale text-scale-mode-amount))
                                         pr-buffers-data))
                             (pr-render-buffer buf data)))))
    (progn (erase-buffer)
           (insert (km_get (pr-get-buffer-data (current-buffer))
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
                 nil (lambda (pr) (km_upd pr :text-scale (lambda (s) (- (or s 0) 1))))))
       :n "K" (lambda ()
                (interactive)
                (pr-upd-buffer-data
                 nil (lambda (pr) (km_upd pr :text-scale (lambda (s) (+ (or s 0) 1))))))
       :n "H" (lambda ()
                (interactive)
                (pr-upd-buffer-data
                 nil (lambda (pr)
                       (km_upd pr (list :pr-data :resolution)
                               (lambda (r) (- (or r 32) 4))))))
       :n "L" (lambda ()
                (interactive)
                (pr-upd-buffer-data
                 nil (lambda (pr)
                       (km_upd pr (list :pr-data :resolution)
                               (lambda (r) (+ (or r 32) 4))))))))
