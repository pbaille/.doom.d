;;; elisp-cookbook/udp-message.el -*- lexical-binding: t; -*-

(progn :chatGPT-explains-how-to-send-message-to-udp-socket
       ;; Define the IP address and port number of the UDP socket
       (setq ip-address "127.0.0.1")
       (setq port-number 9004)

       ;; Create a UDP socket
       (setq udp-socket (make-network-process :name "udp-socket" :type 'datagram :family 'ipv4 :host ip-address :service port-number))

       ;; Define the string to send to the socket
       (setq message "Hello, world!")

       ;; Convert the string to a byte array
       (setq message-bytes (encode-coding-string message 'utf-8))
       ;; Send the byte array to the socket
       (process-send-string udp-socket message-bytes))
