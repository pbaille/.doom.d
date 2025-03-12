;; -*- no-byte-compile: t; lexical-binding: t; -*-

(require 'request)
(require 'json)

(progn
  (defvar pb-llm_chats
    ())

  (defun pb-llm_create-chat (id)
    (setq pb-llm_chats
          (km_put pb-llm_chats
                  id
                  (km :options
                      (km :url "https://api.anthropic.com/v1/messages"
                          :api-key pb/claude-api-key
                          :model "claude-3-7-sonnet-20250219"
                          :max-tokens 10000)

                      :messages
                      (list (km :role "user"
                                :content "You are firendly LLM"))))))

  (defun pb-llm_chat (chat-id prompt)
    (pb_if [(km_keys options messages) (km_get pb-llm_chats chat-id)]
           (pb_let [(km_keys url api-key max-tokens model) options
                    data (json-encode
                          (km :model model
                              :max_tokens max-tokens
                              :messages (vconcat messages
                                                 (list (km :role "user"
                                                           :content prompt)))))]
               (request
                 url
                 :type "POST"
                 :headers `(("Content-Type" . "application/json")
                            ("x-api-key" . ,api-key)
                            ("anthropic-version" . "2023-06-01"))
                 :data (json-encode
                        (km :model model
                            :max_tokens max-tokens
                            :messages (vconcat messages
                                               (list (km :role "user"
                                                         :content prompt)))))
                 :parser #'json-read
                 :success (lambda (&rest args)
                            (pb_let [response
                                     (pb-> (km_get args :data)
                                           (km_from-alist)
                                           (km_select-paths :role :content)
                                           (km_upd :content
                                                   (pb_fn [content]
                                                          (vconcat (mapcar #'km_from-alist content)))))]
                                (setq pb-llm_chats
                                      (km_upd pb-llm_chats
                                              (list chat-id :messages)
                                              (pb_fn [messages]
                                                     (vconcat messages
                                                              (list response)))))))
                 :error (cl-function
                         (lambda (&key data &allow-other-keys)
                           (message "Error calling Anthropic API: %S" data))))
             :ok)
           (progn
             (pb-llm_create-chat chat-id)
             (pb-llm_chat chat-id prompt)))))

(pb_comment
 (pb-llm_create-chat :first)
 (pb-llm_chat :first
              "I'd like to speak about common lisp")

 (pb-llm_chat :first
              "maybe how it compares to clojure")

 (km_get pb-llm_chats :first)

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
