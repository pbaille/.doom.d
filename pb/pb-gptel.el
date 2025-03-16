;;; pb-gptel.el --- GPT integration utilities for Emacs -*- lexical-binding: t; -*-

;; Author: Pierre Baille
;; URL: https://github.com/pbaille
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; This package provides utility functions to enhance the interaction
;; with the gptel package, making it easier to create new GPT sessions
;; and interact with the current buffer.

;;; Code:

(require 'pb-misc)
(require 'gptel)
(require 'gptel-context)
(require 'pb-tree)
(require 'pb-prompt)
(require 'km)
(require 'symex)
(require 'pb-symex)

(defvar pb-gptel_history-dir
  "~/.doom.d/chats/history/")

(defvar pb-gptel_buffer-name-prefix
  "GPTEL_")

(defun pb-gptel_request (content options)
  "Send GPT request with formatted CONTENT and OPTIONS.
CONTENT is a plist or keyword map that will be formatted into an XML-like
prompt string using `pb-gptel_mk-prompt`.
OPTIONS is a plist of options to be passed to `gptel-request`.

When called interactively, this prompts for the necessary inputs."
  (interactive)
  (apply #'gptel-request
         (pb-prompt_mk content)
         options))

(defun pb-gptel_mk-request-prompt (path)
  "Generate a formatted prompt string from the values at PATH in `pb-prompt_tree`.

PATH is a list of keywords representing a path in the prompt tree structure.
For example, [:code :lisp :context] would retrieve all values along that path.

The function retrieves all values along the specified path using
`pb-tree_get-path-values`, then formats each value using `pb-prompt_mk`,
and finally concatenates them with double newlines between each part.

This creates a well-structured prompt by combining multiple prompt elements
from the tree, allowing for modular and reusable prompt components."
  (pb_let [values (pb-tree_get-path-values pb-prompt_tree path)]
    (mapconcat #'pb-prompt_mk values "\n\n")))


(progn :context

       (defun pb-gptel_context-files-to-km ()
         "Convert gptel context files to a keyword map structure.
Transforms the `gptel-context--alist` into a keyword map where:
- Each key is the filename (without directory) of a context file
- Each value is the content of that file as a string

This creates a structured representation of all files currently
added to the gptel context, making it easy to include them in prompts."
         (km_into ()
                  (mapcar (lambda (x)
                            (let ((filepath (car x)))
                              (cons (file-name-nondirectory filepath)
                                    (with-temp-buffer (insert-file-contents filepath) (buffer-string)))))
                          gptel-context--alist)))

       (defun pb-gptel_remove-context-file ()
         "Let the user choose a file present in context using gptel-context-remove.
This function presents a completion interface for selecting a file to remove
from the current gptel context."
         (interactive)
         (let* ((context-files (mapcar #'car gptel-context--alist))
                (selected-file (completing-read "Remove file from context: " context-files nil t)))
           (when selected-file
             (gptel-context-remove selected-file)
             (message "Removed %s from context" selected-file)))))

(defun pb-gptel_current-symex-request-handler (res info)
  "Replace current symbolic expression with GPT model response.
RES is the response text from the language model.
INFO is the information plist provided by gptel containing request metadata.
Performs proper navigation and cleanup after replacement."
  (symex-change 1)
  (insert res)
  (symex-mode-interface)
  (symex-tidy))

(defun pb-gptel_current-symex-request (&optional options)
  "Request a language model to rewrite the current symbolic expression.

OPTIONS is a plist or keyword map that may contain:
- `prompt`: A string with instructions for the language model (prompted
  for interactively if not provided)
- `callback`: A function to handle the response (defaults to
  `pb-gptel_replace-current-symex-request-handler` which replaces
  the current expression)

This function creates a structured request with the current buffer context,
the specified prompt, and the current symbolic expression, then sends it
to the language model using `pb-gptel_request`.

When called interactively, prompts for instructions to guide the modification."
  (interactive)
  (pb_let [(km_keys prompt callback) options]
          (pb-gptel_request

           (km :context
               (km :current-file
                   (km :editor "emacs"
                       :buffer-name (buffer-file-name)
                       :major-mode (symbol-name major-mode)
                       :file-content (buffer-substring-no-properties (point-min) (point-max)))
                   :additional-files
                   (pb-gptel_context-files-to-km))
               :instructions
               (km :base "You are a useful code assistant, you really like lisp-like languages and you know how to balance parentheses correctly."
                   :response-format ["Your response should be valid code, intended to replace the current expression in a source code file."
                                     "Don't use markdown code block syntax or any non-valid code in your output."]
                   :expression (pb-symex_current-as-string)
                   :task (or prompt
                             (read-string "Edit current expression: "))))

           (km :callback
               (or callback
                   #'pb-gptel_current-symex-request-handler)))))

(defun pb-gptel_fill-holes ()
  "Fill holes (denoted by __) in the current expression.
This function sends the current expression to the language model with
instructions to complete any placeholder holes marked with __ without
modifying the rest of the code. The response will replace the current
expression."
  (interactive)
  (pb-gptel_current-symex-request
   (km :prompt "Complete the holes (denoted by __) in the given expression, do not change anything else!")))

(defun pb-gptel_current-buffer-chat (&optional options)
  "Create a chat buffer to discuss code with GPT based on OPTIONS.

OPTIONS is a plist or keyword map that may contain:
- `prompt': A string with instructions for the language model
   (prompted for interactively if not provided)
- `selection': The code excerpt to discuss (defaults to region if active,
   otherwise current symbolic expression if not provided)

This function creates a dedicated chat buffer in org-mode with the
following structure:
1. A level-1 header with the file name
2. A code block containing the selected code (when provided)
3. A level-2 header with the prompt/question
4. The GPT response formatted as org content

The chat buffer is set up for further interaction with the model using
gptel-mode, allowing for back-and-forth conversation about the code.
After the GPT response is inserted, a new level-2 header is added
to continue the conversation."
  (interactive)
  (pb_let [(km_keys prompt selection) options]
          (let* ((buffer-file-name (buffer-file-name))
                 (file-name (if buffer-file-name
                                (file-name-nondirectory buffer-file-name)
                              (buffer-name)))
                 (chat-buffer-name (concat "CHAT_" file-name))
                 (major-mode-name (symbol-name major-mode))
                 (prompt (or prompt (read-string (format "Chat about %s: " file-name ))))
                 (selection (or selection
                                (when (use-region-p)
                                  (buffer-substring-no-properties
                                   (region-beginning)
                                   (region-end))))))

            ;; Create or switch to the chat buffer
            (with-current-buffer (get-buffer-create chat-buffer-name)
              (org-mode)
              (erase-buffer)
              (gptel-mode)

              ;; Insert the symex as a code block
              (insert (format "* %s\n\n" file-name))
              (when selection
                (insert (format "#+begin_src %s\n%s\n#+end_src\n\n"
                                (replace-regexp-in-string "-mode$" "" major-mode-name)
                                selection)))
              (insert (concat "** " prompt "\n\n" ))

              ;; Switch to the buffer and position cursor
              (switch-to-buffer (current-buffer))
              (goto-char (point-max))
              (evil-insert-state))

            ;; Send request to get response
            (pb-gptel_request

             (km :context
                 (km :current-file
                     (km :editor "emacs"
                         :buffer-name (buffer-file-name)
                         :major-mode (symbol-name major-mode)
                         :file-content (buffer-substring-no-properties (point-min) (point-max)))
                     :additional-files
                     (pb-gptel_context-files-to-km))
                 :instructions
                 (km :base "You are a useful code assistant, you really like lisp-like languages and you know how to balance parentheses correctly."
                     :response-format ["Your response will be inserted in an org buffer, it should be valid org content, you can use headers starting at level 3 (***)."
                                       "Org code blocks should use the syntax: #+begin_src <lang>\n<code block content>\n#+end_src"]
                     :expression selection
                     :task prompt))

             (km :callback
                 (lambda (res _)
                   (with-current-buffer chat-buffer-name
                     (goto-char (point-max))
                     (insert res)
                     (insert "\n\n**  ")
                     (evil-normal-state))))))))

(defun pb-gptel_current-symex-chat ()
  "Create a chat buffer to discuss the current symbolic expression.
This function opens a dedicated chat buffer in org-mode containing
the current symbolic expression and initiates a conversation with GPT.
The user will be prompted to enter a question or topic for discussion
about the expression. The resulting buffer will contain:

1. A header with the current file name
2. A code block with the current symbolic expression
3. The user's question/prompt
4. GPT's response formatted as org content

The chat buffer supports ongoing conversation through gptel-mode."
  (interactive)
  (pb-gptel_current-buffer-chat
   (km :selection (pb-symex_current-as-string)
       :prompt (read-string "Chat about current expression: "))))

(defun pb-gptel_new-session-above ()
  "Create a new GPT session in a split window above the current one.
If current buffer visits a file, the new buffer will point to a file with
same extension, that will be created under `pb-gptel-history-dir'."
  (interactive)
  (split-window-vertically)
  (windmove-down)
  (if (buffer-file-name)
      (let* ((current-file-name (buffer-file-name))
             (file (expand-file-name
                    (file-name-nondirectory current-file-name)
                    pb-gptel_history-dir)))
        (let ((buffer-name
               (concat pb-gptel_buffer-name-prefix
                       (file-name-nondirectory file))))
          (if (file-exists-p file)
              (find-file file)
            (progn
              (switch-to-buffer (get-buffer-create buffer-name))
              (set-visited-file-name file)))
          (setq-local gptel--system-message
                      (concat "You are a useful code assistant, that helps discussing "
                              current-file-name
                              " your ourput will only be valid syntax suitable to be inserted in this kind of source code file."))
          (gptel-context-add-file current-file-name)))
    (with-current-buffer (get-buffer-create
                          (concat pb-gptel_buffer-name-prefix
                                  (buffer-name)))
      (switch-to-buffer (current-buffer))
      (org-mode)
      (gptel-mode)
      (insert "*** ")
      (goto-char (point-max))
      (evil-insert-state))))

(provide 'pb-gptel)
;;; pb-gptel.el ends here.
