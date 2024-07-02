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

(pb_defun pr-render-grid [(and pr (km_keys bars pitch-range faces resolution))]
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

(defun pr-read-plist-from-file (filename)
  (with-temp-buffer
    (insert-file-contents filename)
    (read (current-buffer))))

(defvar-local pr-local-data ())

(define-minor-mode proll-mode
  "PRoll"
  :lighter "PRoll"
  (if proll-mode
      (progn (print "proll-mode enabled")
             (let* ((content (buffer-substring-no-properties (point-min) (point-max)))
                    (data (eval (read content) t)))
               (fundamental-mode)
               (setq proll-mode t)
               (setq pr-local-data content)
               (delete-region (point-min) (point-max))
               (insert (pr-render data))
               (text-scale-set -3)))
    (progn (print "proll-mode disabled")
           (delete-region (point-min) (point-max))
           (insert pr-local-data)
           (emacs-lisp-mode)
           (setq pr-local-data ()))))

(defvar pr-sample-data
  (let* (;; colors
         (bg-light "gray90")
         (bg-dark "gray80")
         (bg-note (pb-color_hsl .95 .6 .6))
         (grid-border-color "gray95"))

    (km :pitch-range (cons 58 74)
        :resolution 32
        :bars (list (km :beat 1 :length 4)
                    (km :beat 1 :length 2)
                    (km :beat 1 :length 3)
                    (km :beat 1 :length 3))
        :notes (list (km :pitch 60 :position 0 :duration 2)
                     (km :pitch 64 :position 2 :duration 4)
                     (km :pitch 67 :position 6 :duration 1)
                     (km :pitch 72 :position 7 :duration 1)
                     (km :pitch 69 :position 8 :duration 3)
                     (km :pitch 66 :position 8 :duration 3))
        :faces (km :line (lambda (pitch)
                           (let* ((is-light (member (mod pitch 12) (list 0 2 4 5 7 9 11))))
                             (km :box (km :line-width (cons 0 1) :color grid-border-color)
                                 :background (if is-light bg-light bg-dark))))
                   :note (lambda (note)
                           (km :background bg-note))
                   :bar (lambda (bar face)
                          (km_upd face :background
                                  (lambda (bg)
                                    (if (cl-oddp (km_get bar :idx))
                                        (pb-color_darken bg .05)
                                      bg))))))))

(with-current-buffer (get-buffer-create "*pr*")
  (delete-region (point-min) (point-max))
  (insert (pr-render pr-sample-data))
  (text-scale-set -3))

(let* (;; colors
       (bg-light "gray90")
       (bg-dark "gray80")
       (bg-note (pb-color_hsl .95 .6 .6))
       (grid-border-color "gray95")
       ;; spec
       (pr (km_merge (pr-read-plist-from-file "~/Code/WIP/noon/src/noon/doc/sample-pr.el")
                     (km :pitch-range (cons 48 84)
                         :resolution 32
                         :faces (km :line (lambda (pitch)
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
                                                       bg)))))))))
  (with-current-buffer (get-buffer-create "*pr*")
    (delete-region (point-min) (point-max))
    (insert (pr-render pr))
    (text-scale-set -3.5)))
