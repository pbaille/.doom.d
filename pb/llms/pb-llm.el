;; -*- no-byte-compile: t; lexical-binding: t; -*-

(require 'request)
(require 'json)
(require 'vec)
(require 'secrets)

;;; Code:

(progn
  (defvar pb-llm/chats
    ())

  (defvar pb-llm/last-responses
    ())

  (defun pb-llm/create-chat (id system-prompt)
    (pb/setq pb-llm/chats
             (km/put _
                     id
                     (km :options
                         (km :url "https://api.anthropic.com/v1/messages"
                             :api-key pb/claude-api-key
                             :model "claude-3-7-sonnet-20250219"
                             :max-tokens 10000)

                         :system
                         (vector
                          (km :type "text"
                              :text system-prompt
                              :cache_control (km :type "ephemeral")))

                         :messages ()))))

  (pb-llm/create-chat :simple
                      "You are a useful assistant, you leave in emacs.")

  (defun pb-llm/cache-context (id prompt)
    "Add a new element to the :messages entry of the ID chat,")

  (defun pb-llm/chat (chat-id prompt)
    (pb/if [chat (km/get pb-llm/chats chat-id)]
           (pb/let [(km/keys options system messages) chat
                    (km/keys url api-key max-tokens model) options
                    data (json-encode
                          (km :model model
                              :max_tokens max-tokens
                              :messages (vec_conj messages
                                                  (km :role "user"
                                                      :content prompt))))]
             (request
               url
               :type "POST"
               :headers `(("Content-Type" . "application/json")
                          ("x-api-key" . ,api-key)
                          ("anthropic-version" . "2023-06-01"))
               :data (json-encode
                      (km :model model
                          :max_tokens max-tokens
                          :system system
                          :messages (vec_conj messages
                                              (km :role "user"
                                                  :content prompt))))
               :parser #'json-read
               :success (lambda (&rest args)
                          (pb/setq pb-llm/last-responses
                                   (cons (km/get args :data) _))
                          (pb/let [response
                                   (pb-> (km/get args :data)
                                         (km/from-alist)
                                         (km/select-paths :role :content)
                                         (km/upd :content
                                                 (pb/fn [content]
                                                        (vec (mapcar #'km/from-alist content)))))]
                            (pb/setq pb-llm/chats
                                     (km/upd _
                                             (list chat-id :messages)
                                             (pb/fn [messages]
                                                    (vec_conj messages
                                                              response))))))
               :error (cl-function
                       (lambda (&key data &allow-other-keys)
                         (message "pb-llm/chat request error: %S"
                                  data))))
             :ok)

           (progn
             (pb-llm/create-chat chat-id "You are a useful assistant, you leave in emacs.")
             (pb-llm/chat chat-id prompt)))))

(pb/comment
 (km/get pb-llm/chats :simple)
 (km/get pb-llm/chats :pb-llm)
 pb-llm/last-responses

 (pb-llm/create-chat :pb-llm
                     (pb-gptel/mk-request-prompt [:code :lisp :context]))

 (pb-llm/chat :pb-llm
              "Explain the pb-llm package purpose")

 (pb-llm/chat :simple
              "I'd like to speak about common lisp")

 (pb-llm/chat :simple
              "maybe how it compares to clojure")

 (json-encode (km/get pb-llm/chats :first)))





;; to be moved

(defun pb/alistp (obj)
  "Return t if OBJ is an association list.
An association list is a list where each element is a cons cell
with a non-nil car part and typically a symbol as the car.
Empty lists are considered alists."
  (or (null obj)
      (and (consp obj)
           (consp (car obj))
           (symbolp (caar obj))
           (pb/alistp (cdr obj)))))

(defun km/from-alist (alist)
  "Convert ALIST to a keyword map recursively.
Each key in the alist is converted to a keyword if it's a symbol.
If a value is an alist, it's recursively converted to a km."

  (km/into ()
           (mapcar (pb/fn [(cons k v)]
                          (cons (pb/keyword k)
                                (if (pb/alistp v)
                                    (km/from-alist v)
                                  v)))
                   alist)))

(cl-assert
 (and
  (equal (km/from-alist '((a . 1)
                          (b . 5)
                          (c . ((d . 6)))))
         '(:a 1 :b 5 :c (:d 6)))

  (pb/alistp '((a . 1)
               (b . 5)
               (c . ((d . 6)))))

  (not (pb/alistp '(4
                    (a . 1)
                    (b . 5)
                    (c . ((d . 6))))))))

(provide 'pb-llm)
