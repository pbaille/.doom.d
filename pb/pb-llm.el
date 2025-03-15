;; -*- no-byte-compile: t; lexical-binding: t; -*-

(require 'request)
(require 'json)
(require 'vec)

(progn
  (defvar pb-llm_chats
    ())

  (defvar pb-llm_last-responses
    ())

  (defun pb-llm_create-chat (id system-prompt)
    (pb_setq pb-llm_chats
             (km_put _
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

  (pb-llm_create-chat :simple
                      "You are a useful assistant, you leave in emacs.")

  (defun pb-llm_cache-context (id prompt)
    "Add a new element to the :messages entry of the ID chat,")

  (defun pb-llm_chat (chat-id prompt)
    (pb_if [chat (km_get pb-llm_chats chat-id)]
           (pb_let [(km_keys options system messages) chat
                    (km_keys url api-key max-tokens model) options
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
                          (pb_setq pb-llm_last-responses
                                   (cons (km_get args :data) _))
                          (pb_let [response
                                   (pb-> (km_get args :data)
                                         (km_from-alist)
                                         (km_select-paths :role :content)
                                         (km_upd :content
                                                 (pb_fn [content]
                                                        (vec (mapcar #'km_from-alist content)))))]
                            (pb_setq pb-llm_chats
                                     (km_upd _
                                             (list chat-id :messages)
                                             (pb_fn [messages]
                                                    (vec_conj messages
                                                              response))))))
               :error (cl-function
                       (lambda (&key data &allow-other-keys)
                         (message "pb-llm_chat request error: %S"
                                  data))))
             :ok)

           (progn
             (pb-llm_create-chat chat-id "You are a useful assistant, you leave in emacs.")
             (pb-llm_chat chat-id prompt)))))

(pb_comment
 (km_get pb-llm_chats :simple)
 (km_get pb-llm_chats :pb-llm)
 pb-llm_last-responses

 (pb-llm_create-chat :pb-llm
                     (pb-gptel_mk-request-prompt [:code :lisp :context]))

 (pb-llm_chat :pb-llm
              "Explain the pb-llm package purpose")

 (pb-llm_chat :simple
              "I'd like to speak about common lisp")

 (pb-llm_chat :simple
              "maybe how it compares to clojure")

 (json-encode (km_get pb-llm_chats :first)))





;; to be moved

(defun pb_alistp (obj)
  "Return t if OBJ is an association list.
An association list is a list where each element is a cons cell
with a non-nil car part and typically a symbol as the car.
Empty lists are considered alists."
  (or (null obj)
      (and (consp obj)
           (consp (car obj))
           (symbolp (caar obj))
           (pb_alistp (cdr obj)))))

(defun km_from-alist (alist)
  "Convert ALIST to a keyword map recursively.
Each key in the alist is converted to a keyword if it's a symbol.
If a value is an alist, it's recursively converted to a km."

  (km_into ()
           (mapcar (pb_fn [(cons k v)]
                          (cons (pb_keyword k)
                                (if (pb_alistp v)
                                    (km_from-alist v)
                                  v)))
                   alist)))

(cl-assert
 (and
  (equal (km_from-alist '((a . 1)
                          (b . 5)
                          (c . ((d . 6)))))
         '(:a 1 :b 5 :c (:d 6)))

  (pb_alistp '((a . 1)
               (b . 5)
               (c . ((d . 6)))))

  (not (pb_alistp '(4
                    (a . 1)
                    (b . 5)
                    (c . ((d . 6))))))))

(provide 'pb-llm)
