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
(require 'pb-clojure)

(defvar pb-gptel/history-dir
  "~/.doom.d/chats/history/")

(defvar pb-gptel/buffer-name-prefix
  "GPTEL_")

(defun pb-gptel/request (content options)
  "Send GPT request with formatted CONTENT and OPTIONS.
   CONTENT is a plist or keyword map that will be formatted into an XML-like
   prompt string using `pb-gptel/mk-prompt`.
   OPTIONS is a plist of options to be passed to `gptel-request`.

   When called interactively, this prompts for the necessary inputs."
  (interactive)
  (let ((gptel-max-tokens 64000))
    (apply #'gptel-request
           (encode-coding-string (pb-prompt/mk content)
                                 'utf-8)
           options)))

(defun pb-gptel/mk-request-prompt (path)
  "Generate a formatted prompt string from the values at PATH in `pb-prompt/tree`.

   PATH is a list of keywords representing a path in the prompt tree structure.
   For example, [:code :lisp :context] would retrieve all values along that path.

   The function retrieves all values along the specified path using
   `pb-tree_get-path-values`, then formats each value using `pb-prompt/mk`,
   and finally concatenates them with double newlines between each part.

   This creates a well-structured prompt by combining multiple prompt elements
   from the tree, allowing for modular and reusable prompt components."
  (pb_let [values (pb-tree_get-path-values pb-prompt/tree path)]
    (mapconcat #'pb-prompt/mk values "\n\n")))


(progn :context

       (defvar pb-gptel/context ())

       (defun pb-gptel/context-files-to-km ()
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

       (defun pb-gptel/directory-to-km (dir-path &optional max-depth current-depth)
         "Convert all files in DIR-PATH into a nested keyword map structure.
          Recursively builds a keyword map where:
          - Keys are the basenames of files and directories
          - File values are the content of those files as strings
          - Directory values are nested keyword maps with their contents

          Arguments:
          DIR-PATH: Directory path to process
          MAX-DEPTH: Optional maximum recursion depth (nil means no limit)
          CURRENT-DEPTH: Internal parameter for tracking recursion depth"
         (let ((current-depth (or current-depth 0))
               (max-depth-reached (and max-depth (>= current-depth max-depth)))
               (result ()))
           (when (and (file-exists-p dir-path)
                      (file-directory-p dir-path)
                      (not max-depth-reached))
             (dolist (item (directory-files dir-path t "^[^.]"))
               (let ((basename (file-name-nondirectory item)))
                 (cond
                  ;; For directories, recurse to create nested km
                  ((file-directory-p item)
                   (setq result (km_put result
                                        (intern (concat ":" basename))
                                        (pb-gptel/directory-to-km item max-depth (1+ current-depth)))))

                  ;; For regular files, add content as value
                  ((file-regular-p item)
                   (setq result (km_put result
                                        (intern (concat ":" basename))
                                        (with-temp-buffer
                                          (insert-file-contents item)
                                          (buffer-string)))))))))
           result))

       (defun pb-gptel/remove-context-files ()
         "Remove multiple files from the current gptel context.
          This function presents a multi-selection interface for choosing
          files to remove from the current gptel context."
         (interactive)
         (let* ((context-files (mapcar #'car gptel-context--alist))
                (selected-files (let ((crm-separator "[ 	]* [ 	]*"))
                                  (completing-read-multiple "Remove files from context: "
                                                            context-files nil t))))
           (when  selected-files
             (dolist (file selected-files)
               (gptel-context-remove file))
             (message "Removed %d file(s) from context: %s"
                      (length selected-files)
                      (mapconcat #'identity selected-files ", ")))))

       (defun pb-gptel/add-context-files ()
         "Add multiple files to the current gptel context.
          This function presents a multi-selection interface for choosing
          files to add to the current gptel context."
         (interactive)
         (let* ((default-directory (or (projectile-project-root)
                                       default-directory))
                (files (let ((crm-separator "[ 	]* [ 	]*"))
                         (completing-read-multiple "Add files to context: "
                                                   ;#'read-file-name-internal
                                                   (projectile-current-project-files)
                                                   nil t))))
           (when files
             (dolist (file files)
               (gptel-context-add-file file))
             (message "Added %d file(s) to context: %s"
                      (length files)
                      (mapconcat #'identity files ", "))))))

(defvar pb-gptel/overlay-color (doom-darken "#232530" 0.4)
  "Color used to highlight the current s-expression when being edited by GPT.
This is a darkened version of the default theme background.")

(defun pb-gptel/current-symex-request-handler (res info)
  "Replace current symbolic expression with GPT model response.
   This function handles the response from a GPT model request and replaces
   the current symbolic expression with that response.

   The function performs the following steps:
   1. Enter change mode for the current symbolic expression
   2. Insert the model's response text in place of the original content
   3. Restore the symbolic expression navigation interface
   4. Tidy up the edited expression (proper formatting, indentation, etc.)

   Parameters:
   - RES: The response text from the language model
   - INFO: A plist containing metadata about the request (provided by gptel)

   This function is used as the default callback for pb-gptel/current-symex-request."
  (set-face-attribute
   'symex--current-node-face nil
   :inherit nil
   :background (doom-lighten "#232530" 0.03))
  (symex-change 1)
  (insert res)
  (symex-mode-interface)
  (symex-tidy))

(defun pb-gptel/current-symex-request (&optional options)
  "Request a language model to rewrite the current symbolic expression.

   OPTIONS is a plist or keyword map that may contain:
   - `prompt`: A string with instructions for the language model (prompted
     for interactively if not provided)
   - `callback`: A function to handle the response (defaults to
     `pb-gptel/replace-current-symex-request-handler` which replaces
     the current expression)

   This function creates a structured request with the current buffer context,
   the specified prompt, and the current symbolic expression, then sends it
   to the language model using `pb-gptel/request`.

   When called interactively, prompts for instructions to guide the modification."
  (interactive)
  (pb_let [(km_keys prompt callback) options]
    (set-face-attribute
     'symex--current-node-face nil
     :inherit nil
     :background pb-gptel/overlay-color)
    (pb-gptel/request

     (km :context
         (km :current-file
             (km :editor "emacs"
                 :buffer-name (buffer-file-name)
                 :major-mode (symbol-name major-mode)
                 :file-content (buffer-substring-no-properties (point-min) (point-max)))
             :additional-files
             (pb-gptel/context-files-to-km))
         :instructions
         (km :base "You are a useful code assistant, you really like lisp-like languages and you know how to balance parentheses correctly."
             :response-format ["Your response should be valid code, intended to replace the current expression in a source code file."
                               "Don't use markdown code block syntax or any non-valid code in your output."
                               "If you have to write prose, use appropriate comment syntax."]
             :expression (pb-symex_current-as-string)
             :task (or prompt
                       (read-string "Edit current expression: "))))

     (km :callback
         (or callback
             #'pb-gptel/current-symex-request-handler)))
    (message "Querying...")))

(defun pb-gptel/current-buffer-request (&optional options)
  "Request a language model to rewrite the current buffer contents.

   OPTIONS is a plist or keyword map that may contain:
   - `prompt`: A string with instructions for the language model (prompted
     for interactively if not provided)
   - `callback`: A function to handle the response (defaults to a
     function that replaces the entire buffer content)

   This function creates a structured request with the current buffer context,
   the specified prompt, and sends it to the language model using
   `pb-gptel/request`. The response will replace the entire buffer content.

   When called interactively, prompts for instructions to guide the modification."
  (interactive)
  (pb_let [(km_keys prompt callback) options]
    (pb-gptel/request
     ;; Create a structured prompt with context information
     (km :context
         (km :current-file
             (km :editor "emacs"
                 :buffer-name (buffer-file-name)
                 :major-mode (symbol-name major-mode)
                 :file-content (buffer-substring-no-properties (point-min) (point-max)))
             :additional-files
             (pb-gptel/context-files-to-km))
         :instructions
         (km :base "You are a useful code assistant, you really like lisp-like languages and you know how to balance parentheses correctly."
             :response-format ["Your response should be valid code, intended to replace the whole content of the current buffer."
                               "Don't use markdown code block syntax or any non-valid code in your output."
                               "If you have to write prose, use appropriate comment syntax."]
             :task (or prompt
                       (read-string "Edit current buffer: "))))

     ;; Define the callback to process the response
     (km :callback
         (lambda (res info)
           (with-current-buffer (plist-get info :buffer)
             (delete-region (point-min) (point-max))
             (insert res)))))))

(defun pb-gptel/fill-holes ()
  "Fill holes (denoted by __) in the current expression.
   This function sends the current expression to the language model with
   instructions to complete any placeholder holes marked with __ without
   modifying the rest of the code. The response will replace the current
   expression."
  (interactive)
  (pb-gptel/current-symex-request
   (km :prompt "Complete the holes (denoted by __) in the given expression, do not change anything else!")))

(defun pb-gptel/current-buffer-chat (&optional options)
  "Create a chat buffer to discuss code with GPT based on OPTIONS.

   OPTIONS is a plist or keyword map that may contain:
   - `prompt': A string with instructions for the language model
   (prompted for interactively if not provided)
   - `selection': The code excerpt to discuss (defaults to region if active,
   otherwise current symbolic expression if not provided)

   This function creates a dedicated chat buffer in `org-mode' with the
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
    (let* ((source-buffer (current-buffer))
           (file-path (buffer-file-name))
           (file-name (if file-path
                          (file-name-nondirectory file-path)
                        (buffer-name)))
           (chat-buffer-name (concat "CHAT_" file-name)))

      (if (get-buffer chat-buffer-name)
          (switch-to-buffer (get-buffer chat-buffer-name))
        (let* ((major-mode-name (symbol-name major-mode))
               (selection (or selection
                              (when (use-region-p)
                                (buffer-substring-no-properties
                                 (region-beginning)
                                 (region-end)))))

               (full-prompt (km :context
                                (km :current-file
                                    (km :editor "emacs"
                                        :buffer-name (buffer-file-name)
                                        :major-mode (symbol-name major-mode)
                                        :file-content (buffer-substring-no-properties (point-min) (point-max)))
                                    :additional-files
                                    (pb-gptel/context-files-to-km))
                                :instructions
                                (km :base "You are a useful code assistant, you really like lisp-like languages and you know how to balance parentheses correctly."
                                    :response-format ["Your response will be inserted in an org buffer, it should be valid org content"
                                                      "All org headings are level 3, 4, 5 ..."
                                                      "Org code blocks should use the syntax: #+begin_src <lang>\n<code block content>\n#+end_src"]
                                    :expression selection))))

          ;; Create or switch to the chat buffer
          (with-current-buffer (get-buffer-create chat-buffer-name)
            (org-mode)
            (erase-buffer)
            (gptel-mode)
            (setq-local gptel--system-message (pb-prompt/mk full-prompt)
                        gptel-use-tools nil
                        gptel-max-tokens 64000)

            (evil-normal-state)
            (symex-mode -1)

            (progn :bindings
                   (evil-define-key nil 'local (kbd "s-q <return>")
                     (lambda () (interactive)
                       (call-interactively #'gptel-send)
                       (evil-insert-newline-below)
                       (goto-char (point-max))))
                   (evil-define-key nil 'local (kbd "s-q b")
                     (lambda () (interactive)
                       (switch-to-buffer source-buffer))))

            ;; Insert the symex as a code block
            (insert (format "* %s\n\n" file-name))
            (when selection
              (insert (format "#+begin_src %s\n%s\n#+end_src\n\n"
                              (replace-regexp-in-string "-mode$" "" major-mode-name)
                              selection)))
            (insert "** ")

            ;; Switch to the buffer and position cursor
            (switch-to-buffer (current-buffer))
            (goto-char (point-max))
            (evil-insert-state)))))))

(defun pb-gptel/directory-chat ()
  (interactive)
  (let* ((dir (read-directory-name "Directory to discuss: "))
         (chat-buffer-name (read-string "Buffer name: ")))

    (if (get-buffer chat-buffer-name)
        (switch-to-buffer (get-buffer chat-buffer-name))
      (let* ((full-prompt (km :context
                              (km :project-files
                                  (pb-gptel/directory-to-km dir))
                              :instructions
                              (km :base "You are a useful code assistant, you really like lisp-like languages and you know how to balance parentheses correctly."
                                  :response-format ["Your response will be inserted in an org buffer, it should be valid org content"
                                                    "All org headings are level 3, 4, 5 ..."
                                                    "Org code blocks should use the syntax: #+begin_src <lang>\n<code block content>\n#+end_src"]))))

        ;; Create or switch to the chat buffer
        (with-current-buffer (get-buffer-create chat-buffer-name)
          (org-mode)
          (erase-buffer)
          (gptel-mode)
          (setq-local gptel--system-message (pb-prompt/mk full-prompt)
                      gptel-use-tools nil
                      gptel-max-tokens 64000)
          (evil-normal-state)
          (symex-mode -1)

          (progn :bindings
                 (evil-define-key nil 'local (kbd "s-q <return>")
                   (lambda () (interactive)
                     (call-interactively #'gptel-send)
                     (evil-insert-newline-below)
                     (goto-char (point-max)))))

          ;; Insert the symex as a code block
          (insert (format "* %s\n\n" dir))
          (insert "** ")

          ;; Switch to the buffer and position cursor
          (switch-to-buffer (current-buffer))
          (goto-char (point-max))
          (evil-insert-state))))))

(defun pb-gptel/current-symex-chat ()
  "Create a chat buffer to discuss the current symbolic expression.
   This function opens a dedicated chat buffer in `org-mode' containing
   the current symbolic expression and initiates a conversation with GPT.
   The user will be prompted to enter a question or topic for discussion
   about the expression. The resulting buffer will contain:

   1. A header with the current file name
   2. A code block with the current symbolic expression
   3. The user's question/prompt
   4. GPT's response formatted as org content

   The chat buffer supports ongoing conversation through gptel-mode."
  (interactive)
  (pb-gptel/current-buffer-chat
   (km :selection (pb-symex_current-as-string))))

(progn :tools

       (defvar pb-gptel/elisp-evaluation-tool
         (gptel-make-tool
          :name "eval_elisp"
          :function (lambda (code)
                      (condition-case err
                          (let ((result (eval (read code))))
                            (format "%S" result))
                        (error (format "Error: %S" err))))
          :description "Evaluates Elisp code and returns the result. Warning: Only use for safe operations."
          :args (list '(:name "code"
                        :type string
                        :description "Elisp code to evaluate"))
          :category "emacs"
          :confirm t))

       (defvar pb-gptel/clojure-evaluation-tool
         (gptel-make-tool
          :name "eval_clojure"
          :function #'pb-clojure_gptel-tool-function
          :description "Evaluates Clojure code in the current CIDER REPL."
          :args (list '(:name "code"
                        :type string
                        :description "Clojure code to evaluate"))
          :category "clojure"
          :confirm t))

       (setq gptel-tools
             (list pb-gptel/elisp-evaluation-tool
                   pb-gptel/clojure-evaluation-tool))

       (progn :incub

              (defvar pb-gptel/file-insert-at-point
                (gptel-make-tool
                 :name "edit_file"
                 :function (lambda (file_path content position)
                             (let ((filepath (expand-file-name file_path)))
                               (if (file-exists-p filepath)
                                   (with-current-buffer (find-file-noselect filepath)
                                     (save-excursion
                                       (cond
                                        ((eq position 'beginning)
                                         (goto-char (point-min)))
                                        ((eq position 'end)
                                         (goto-char (point-max)))
                                        ((numberp position)
                                         (goto-char position))
                                        (t nil))
                                       (insert content)
                                       (save-buffer))
                                     (format "Content inserted at %s in file %s" position filepath))
                                 (format "Error: File %s not found" filepath))))
                 :description "Inserts text at a specified position in a file from the context."
                 :args (list '(:name "file_path"
                               :type string
                               :description "Path to the file to edit, relative to project root")
                             '(:name "content"
                               :type string
                               :description "Content to insert into the file")
                             '(:name "position"
                               :type string
                               :description "Where to insert content: 'beginning', 'end', or line number"))
                 :category "file-editing"
                 :confirm t)
                "A tool that allows editing source files in the current context by inserting content at specified positions.")))

(pb_comment
 :coerce-non-utf-8-char-to-unicode
 ;; gptel curl do not like non-utf-8
 (pb-gptel/request (with-current-buffer "*Embark Collect: consult-imenu - *"
                     (let ((str (buffer-substring-no-properties (point-min) (point-max))))
                       (with-temp-buffer
                         (insert str)
                         (goto-char (point-min))
                         ;; Instead of removing non-UTF-8 characters, replace them with their Unicode representation
                         (while (re-search-forward "[^\x00-\x7F\u0080-\uFFFF]" nil t)
                           (let ((char (match-string 0)))
                             (replace-match (format "\\\\u%04X" (string-to-char char)))))
                         (buffer-string))))
                   (km :callback (lambda (res info)
                                   (print res)))))


(provide 'pb-gptel)
;;; pb-gptel.el ends here.
