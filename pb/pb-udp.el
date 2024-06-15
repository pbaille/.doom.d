;;; pb/pb-udp.el -*- lexical-binding: t; -*-

(require 'json)

(defvar pb-udp_default-state (list :id nil :data ""))

(defun pb-udp_mk-proc-filter (ops)
  "Build a process filter function given an OPS plist."
  (let ((state pb-udp_default-state))
    (lambda (_ string)
      (let ((json-object-type 'plist)
            (json-array-type 'list)
            (json-message (json-read-from-string string)))
        (let ((id (plist-get json-message :id))
              (op (plist-get json-message :op))
              (data (plist-get json-message :data)))
          (when id
            (unless (equal (plist-get state :id) id)
              (setq state pb-udp_default-state))
            (if (and data (not op))
                (setq state
                      (list :id id
                            :data (concat (plist-get state :data) data)))
              (let* ((f (plist-get ops (intern (concat ":" op))))
                     (ret (funcall f (if (equal (plist-get state :id) id)
                                         (plist-get state :data)
                                       data))))
                (setq state pb-udp_default-state)
                ret))))))))

(defun pb-udp_start-listening (host port ops)
  (let ((proc (make-network-process
               :name "my-udp-proc"
               :buffer "*my-udp-proc*"
               :host host
               :service port
               :server t
               :family 'ipv4
               :type 'datagram)))
    (set-process-filter proc (pb-udp_mk-proc-filter ops))
    proc))

'(:tries

  (defvar pb-udp_receive-proc
    (pb-udp_start-listening 'local
                            "8088"
                            (list :print #'print)))

  (delete-process pb-udp_receive-proc)

  (defvar pb-udp_send-proc
    (make-network-process
     :name "*pb-udp_out*"
     :host 'local
     :service "8088"
     :type 'datagram
     :family 'ipv4))

  (delete-process pb-udp_send-proc)

  (process-send-string pb-udp_send-proc
                       (json-encode-plist '(:id 1 :op :print :data "yo")))
  (process-send-string pb-udp_send-proc
                       (json-encode-plist '(:id 2 :data "yo")))
  (process-send-string pb-udp_send-proc
                       (json-encode-plist '(:id 2 :op :print))))

(provide 'pb-udp)
