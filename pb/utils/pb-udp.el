;;; pb/pb-udp.el --- udp helpers -*- lexical-binding: t; -*-

;; Author: Pierre Baille
;; URL: https://github.com/pbaille
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1") (json))

;;; Commentary:

;; udp helpers.

;;; Code:

(require 'json)

(defvar pb-udp/default-state (list :id nil :data ""))

(defun pb-udp/mk-proc-filter (handler)
  "Build a process filter function given an OPS plist."
  (let ((state pb-udp/default-state))
    (lambda (_ string)
      (let ((json-object-type 'plist)
            (json-array-type 'list))
        (let* ((json-message (json-read-from-string string))
               (id (plist-get json-message :id))
               (op (plist-get json-message :op))
               (data (plist-get json-message :data)))
          (when id
            (unless (equal (plist-get state :id) id)
              (setq state pb-udp/default-state))
            (cond ((and data (not op))
                   (setq state
                         (list :id id
                               :data (concat (plist-get state :data) data))))
                  (op
                   (let* ((msg (if (equal (plist-get state :id) id)
                                   (plist-put json-message :data (plist-get state :data))
                                 json-message))
                          (ret (funcall handler msg)))
                     (setq state pb-udp/default-state)
                     ret))
                  (t
                   (setq state pb-udp/default-state)
                   (error (format "bad format msg: %s" json-message))))))))))

(defun pb-udp/start-listening (host port handler)
  (let ((proc (make-network-process
               :name "pb-udp-proc"
               :buffer "*pb-udp-proc*"
               :host host
               :service port
               :server t
               :family 'ipv4
               :type 'datagram)))
    (set-process-filter proc (pb-udp/mk-proc-filter handler))
    proc))

'(:tries

  (delete-process pb-udp/receive-proc)

  (defvar pb-udp/receive-proc
    (pb-udp/start-listening 'local
                            "8088"
                            (lambda (opts)
                              (let ((op (plist-get opts :op))
                                    (data (plist-get opts :data)))
                                (cond ((equal op "print") (print data))
                                      (t (error "unknown op")))))))

  (delete-process pb-udp/send-proc)

  (defvar pb-udp/send-proc
    (make-network-process
     :name "*pb-udp/out*"
     :host 'local
     :service "8088"
     :type 'datagram
     :family 'ipv4))

  (process-send-string pb-udp/send-proc
                       (json-encode-plist '(:id 1 :op :print :data "yo")))
  (process-send-string pb-udp/send-proc
                       (json-encode-plist '(:id 2 :data "yo")))
  (process-send-string pb-udp/send-proc
                       (json-encode-plist '(:id 2 :op :print))))

(provide 'pb-udp)
;;; pb-udp.el ends here.
