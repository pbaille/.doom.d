;;; pb/pb-udp.el -*- lexical-binding: t; -*-

(defconst pb-udp_BEG_DEL "<UDP|")
(defconst pb-udp_END_DEL "|UDP>")

(defun pb-udp_last-chunk-p (s)
  (string= pb-udp_END_DEL (substring s (- (length s) (length pb-udp_END_DEL)))))

(defun pb-udp_first-chunk-p (s)
  (string= pb-udp_BEG_DEL (substring s 0 (length pb-udp_BEG_DEL))))

(defun pb-udp_chunks-to-string (xs)
  (let ((ret ""))
    (dolist (v xs)
      (setf ret (concat ret v)))
    (substring ret (length pb-udp_BEG_DEL) (- (length ret) (length pb-udp_END_DEL)))))

(defun pb-udp_decode-chunks (xs on-success on-error)
  (let ((str (pb-udp_chunks-to-string xs)))
    (condition-case err
        (let* ((json-object-type 'plist)
               (json-array-type 'list)
               (decoded (json-read-from-string str)))
          (funcall on-success decoded))
      (error
       (funcall on-error `(:type :encode :error ,(error-message-string err) :message ,str))))))

(defvar pb-udp_input-data nil)

(defun pb-udp_listen-filter (proc string)
  (print (format "Received string: %s" string))
  (cond ((pb-udp_first-chunk-p string)
         (setq pb-udp_input-data (list string)))
        (pb-udp_input-data
         (setq pb-udp_input-data (append pb-udp_input-data (list string)))))
  (when (and pb-udp_input-data (pb-udp_last-chunk-p string))
    ;; on-success and on-error are placeholder functions to be replaced
    (pb-udp_decode-chunks pb-udp_input-data
                          (lambda (x) (print (cons 'success x)))
                          (lambda (x) (print (cons 'error x))))
    (setq pb-udp_input-data nil)))

(defun pb-udp_listen-sentinel (proc string)
  (print (format "Process: %s had the event -- %s" proc string)))

(defun pb-udp_start-listening (host port)
  (make-network-process
   :name "my-udp-proc"
   :buffer "*my-udp-proc*"
   :host host
   :service port
   :server t
   :family 'ipv4
   :type 'datagram
   :filter #'pb-udp_listen-filter
   :sentinel #'pb-udp_listen-sentinel))

'(tries
  (pb-udp_start-listening 'local "8083")
  (defvar pb-udp_send-proc
    nil)
  (setq pb-udp_send-proc
        (make-network-process
         :name "*pb-udp_out*"
         :host 'local
         :service "55551"
         :type 'datagram
         :family 'ipv4))
  (process-send-string pb-udp_send-proc
                       "<UDP|{\"io\"")
  (process-send-string pb-udp_send-proc
                       ": 2}|UDP>"))
