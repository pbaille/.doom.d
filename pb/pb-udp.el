;;; pb/pb-udp.el -*- lexical-binding: t; -*-

(require 'json)

(defvar pb-udp_default-state (list :id nil :data ""))

(defun pb-udp_mk-proc-filter (handler)
  "Build a process filter function given an OPS plist."
  (let ((state pb-udp_default-state))
    (lambda (_ string)
      (let ((json-object-type 'plist)
            (json-array-type 'list))
        (let* ((json-message (json-read-from-string string))
               (id (plist-get json-message :id))
               (op (plist-get json-message :op))
               (data (plist-get json-message :data)))
          (when id
            (unless (equal (plist-get state :id) id)
              (setq state pb-udp_default-state))
            (cond ((and data (not op))
                   (setq state
                         (list :id id
                               :data (concat (plist-get state :data) data))))
                  (op
                   (let* ((msg (if (equal (plist-get state :id) id)
                                   (plist-put json-message :data (plist-get state :data))
                                 json-message))
                          (ret (funcall handler msg)))
                     (setq state pb-udp_default-state)
                     ret))
                  (t
                   (setq state pb-udp_default-state)
                   (error (format "bad format msg: %s" json-message))))))))))

(defun pb-udp_start-listening (host port handler)
  (let ((proc (make-network-process
               :name "pb-udp-proc"
               :buffer "*pb-udp-proc*"
               :host host
               :service port
               :server t
               :family 'ipv4
               :type 'datagram)))
    (set-process-filter proc (pb-udp_mk-proc-filter handler))
    proc))

'(:tries

  (delete-process pb-udp_receive-proc)

  (defvar pb-udp_receive-proc
    (pb-udp_start-listening 'local
                            "8088"
                            (lambda (opts)
                              (let ((op (plist-get opts :op))
                                    (data (plist-get opts :data)))
                                (cond ((equal op "print") (print data))
                                      (t (error "unknown op")))))))

  (delete-process pb-udp_send-proc)

  (defvar pb-udp_send-proc
    (make-network-process
     :name "*pb-udp_out*"
     :host 'local
     :service "8088"
     :type 'datagram
     :family 'ipv4))

  (process-send-string pb-udp_send-proc
                       (json-encode-plist '(:id 1 :op :print :data "yo")))
  (process-send-string pb-udp_send-proc
                       (json-encode-plist '(:id 2 :data "yo")))
  (process-send-string pb-udp_send-proc
                       (json-encode-plist '(:id 2 :op :print))))

(provide 'pb-udp)
