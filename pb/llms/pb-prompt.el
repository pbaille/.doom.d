;;; pb-prompt.el --- crafting prompts for LLMs -*- lexical-binding: t; -*-

;;; Commentary:
;; This package provides tools for creating and managing prompts for large
;; language models (LLMs).  It allows users to create structured prompts
;; with a tree-based organization system, making it easier to craft
;; context-specific interactions with LLMs.
;;
;; The main features include:
;; - Prompt formatting with the `pb-prompt/mk` function
;; - File and directory description functionality
;; - A tree-based prompt organization system

;;; Code:

(require 'km)
(require 'pb)
(require 'pb-tree)
(require 'pb-symex)

(defvar pb-prompt/context ())

(defvar pb-prompt/saved-contexts
  (make-hash-table :test 'equal))

(defvar-local pb-prompt/context-browser-focus nil)

(defvar pb-prompt/tree

  (pb-tree "You are a useful assistant that lives in the holly emacs editor."
           :code
           (node ["You are a useful code assistant."
                  "Your response should be valid code, intended to replace the current expression in a source code file."
                  "Don't use markdown code block syntax or any non-valid code in your output."]

                 :lisp
                 (node "You really like lisp-like languages and you know how to balance parentheses correctly."

                       :clj
                       "You are a Clojure expert who understands functional programming concepts and persistent data structures."

                       :cljs
                       ["You are a ClojureScript expert who understands both Clojure concepts and JavaScript interoperability."
                        "You're familiar with the React paradigm and modern frontend development patterns."]

                       :elisp
                       (node ["Guide the Emacs Lisp code assistant to write efficient, clean, and idiomatic code, including implementing functions, leveraging built-in Emacs Lisp libraries, and optimizing for readability and maintainability."
                              "Encourage balancing parentheses and ensuring syntactic correctness throughout the code. Promote using docstrings and comments where necessary to enhance code clarity and understanding."]

                             :context
                             (node "Additional context"
                                   :pb (lambda ()
                                         (km :pb (pb/slurp "~/.doom.d/pb/pb.el")
                                             :km (pb/slurp "~/.doom.d/pb/km.el")))))

                       :context
                       (lambda ()
                         "Include code context, whole file, current-expression and more..."
                         (km :buffer-name (buffer-file-name)
                             :major-mode (symbol-name major-mode)
                             :file-content (buffer-substring-no-properties (point-min) (point-max))
                             :current-expression (pb-symex/current-as-string))))

                 :fill
                 "Complete the holes (denoted by __) in the given expression, do not change anything else!")

           :task (lambda ()
                   "Enter main instructions."
                   (km :task (read-string "Task: ")))))

(progn :prompt-string

       (defun pb-prompt/indent-content (content &optional indent-size)
         "Indent each line of CONTENT with spaces if it contains newlines.
          If CONTENT is a single line, return it unchanged.
          Optional argument INDENT-SIZE specifies the number of spaces to use (defaults to 2)."
         (when content
           (if (string-match-p "\n" content)
               (let ((spaces (make-string (or indent-size 2) ?\s)))
                 (replace-regexp-in-string
                  "^\\(.\\)" (concat spaces "\\1") content))
             content)))

       (defun pb-prompt/mk (x)
         "Generate a formatted prompt based on input X.

          This function processes X, which can be a string, function,
          keyword map, vector, or list, and returns a formatted string
          accordingly:

          - If X is a string, it returns X as-is.
          - If X is a function, it calls the function and processes the
          result recursively.
          - If X is a keyword map, it formats each key-value pair into
          XML-like tags, with multiline content indented for better
          readability.
          - If X is a vector, it concatenates the elements separated by
          newlines.
          - If X is a list, it converts the list to a string representation."
         (cond ((null x) "nil")
               ((stringp x) (substring-no-properties x))
               ((functionp x) (pb-prompt/mk (funcall x)))
               ((km? x)
                (mapconcat (lambda (entry)
                             (let* ((key-str (substring (symbol-name (car entry)) 1))
                                    (content (pb-prompt/mk (cdr entry))))
                               ;; Format each entry as XML-like tags with indented content
                               (concat "<" key-str ">\n"
                                       (pb-prompt/indent-content content 2)
                                       "\n</" key-str ">")))
                           (km/entries x)
                           "\n\n"))          ; Execute functions to get their content
               ((vectorp x) (mapconcat #'identity x "\n"))
               ((listp x) (mapconcat (lambda (item)
                                       (concat "<context-item>\n"
                                               (pb-prompt/indent-content
                                                (pb-prompt/mk item)
                                                2)
                                               "\n</context-item>"))
                                     x
                                     "\n"))
               ((or (booleanp x)
                    (numberp x))
                (format "%s" x))))

       (defun pb-prompt/describe-path (path)
         "Create a structured representation of a file or directory at PATH.
          When PATH is a directory, recursively creates a nested structure that
          includes all non-hidden files and subdirectories.

          For directories, returns a keyword map with the following keys:
          - :path - the full path
          - :filename - the name of the directory without parent path
          - :file-type - always \"directory\"
          - :children - a list of similar structures for each child node

          For files, returns a keyword map with the following keys:
          - :path - the full path
          - :filename - the name of the file without parent path
          - :file-extension - the file extension if any
          - :file-type - always \"file\"
          - :content - the file content as a string

          Returns nil if PATH does not exist or is nil."
         (if (and path (file-exists-p path))
             (if (file-directory-p path)
                 (km :name (file-name-nondirectory (if (and path (string-match-p "/$" path))
                                                       (substring path 0 -1)
                                                     path))
                     :type "dir"
                     :path path
                     :children (seq-reduce (lambda (ret p)
                                             (pb/let [(as x (km/keys name))
                                                      (pb-prompt/describe-path p)]
                                               (if name
                                                   (km/put ret (pb/keyword name) x)
                                                 ret)))
                                           (directory-files path t "^[^.].*")
                                           ()))
               (km :name (file-name-nondirectory path)
                   :type "file"
                   :path path
                   :content (pb/slurp path)))))

       (defun pb-prompt/context-km (context)
         (km :context
             (mapcar
              (lambda (ctx-item)
                (when (not (km/get ctx-item :disabled))
                  (cl-case (intern (km/get ctx-item :type))
                    (buffer
                     (km/put ctx-item
                             :content (with-current-buffer (km/get ctx-item :buffer-name)
                                        (buffer-substring-no-properties (point-min) (point-max)))))

                    (context
                     (km/put ctx-item
                             :content
                             (pb-prompt/context-km
                              (pb-prompt/get-saved-context
                               (km/get ctx-item :name)))))

                    ((file dir)
                     (pb-prompt/describe-path (km/get ctx-item :path)))

                    (function
                     (call-interactively (km/get ctx-item :function)))

                    (url
                     (eww-browse-url (km/get ctx-item :url)))

                    (otherwise ctx-item))))
              context)))

       (defun pb-prompt/context-prompt (&optional context)
         "Generate a prompt from the current context.
          This function formats the collected context elements into a structured
          prompt suitable for an LLM, using the pb-prompt/mk function."
         (interactive)
         (pb-prompt/mk
          (pb-prompt/context-km (or context
                                    pb-prompt/context))))

       (defun pb-prompt/display-context-prompt ()
         (interactive)
         (let ((buffer (get-buffer-create "*pb-prompt/context*")))
           (with-current-buffer buffer
             (xml-mode)
             (flycheck-mode -1)
             (erase-buffer)
             (insert (pb-prompt/context-prompt)))
           (pop-to-buffer buffer)))

       (defun pb-prompt/display-context ()
         (interactive)
         (pb-elisp/display-expression pb-prompt/context
                                      #'km/pp)))

(progn :context-add

       (defun pb-prompt/with-id (context-item)
         "Add a unique identifier to CONTEXT-ITEM.

          This function adds a unique identifier key to the provided keyword map.
          The identifier is constructed by creating an MD5 hash of the current timestamp
          and taking its first 8 characters, prefixed with 'pb-prompt/context-item-'.

          The identifier helps track and reference context items individually within
          the prompt system.

          Argument CONTEXT-ITEM is a keyword map to which the ID will be added.

          Returns the CONTEXT-ITEM with the unique ID added."
         (km/put context-item
                 :id
                 (let ((data-string (format "%s" context-item)))
                   (substring (md5 data-string) 0 8))))

       (defun pb-prompt/update-focused-context (f)
         "Apply function F to update either focused saved context or current context.

          If `pb-prompt/context-browser-focus' is non-nil, applies F to the
          corresponding saved context and stores the result back in the
          `pb-prompt/saved-contexts' hash table.

          If `pb-prompt/context-browser-focus' is nil, applies F to the current
          `pb-prompt/context' and updates it with the result.

          Argument F should be a function that takes a context (list of items) and
          returns a modified context."
         (if pb-prompt/context-browser-focus
             (when-let ((focused-context
                         (gethash pb-prompt/context-browser-focus
                                  pb-prompt/saved-contexts)))
               (puthash pb-prompt/context-browser-focus
                        (funcall f focused-context)
                        pb-prompt/saved-contexts))
           (setq pb-prompt/context
                 (funcall f pb-prompt/context))))

       (defun pb-prompt/add-context-item (context item)
         "Add ITEM to CONTEXT if an item with the same ID doesn't exist.

          This function ensures that no duplicate items (based on :id) are added
          to the context. If ITEM doesn't have an :id, it's wrapped with
          pb-prompt/with-id before being added.

          Returns the updated context with the item added (if it wasn't a duplicate)."
         (let* ((item-with-id (if (km/get item :id)
                                  item
                                (pb-prompt/with-id item)))
                (item-id (km/get item-with-id :id)))
           (if (seq-find (lambda (ctx-item)
                           (equal (km/get ctx-item :id) item-id))
                         context)
               ;; If item with same ID exists, return unchanged context
               context
             ;; Otherwise, add the item to the context
             (cons item-with-id context))))

       (defun pb-prompt/add-item! (item)
         "Add ITEM to the focused context with a unique ID.

          This function adds the specified ITEM to either the current context or a
          focused saved context, depending on the value of `pb-prompt/context-browser-focus`.
          It wraps the item with a unique ID using `pb-prompt/with-id` before adding.

          If `pb-prompt/context-browser-focus` is non-nil, adds the item to the specified
          saved context. Otherwise, adds it to the current `pb-prompt/context`.

          Argument ITEM should be a keyword map (km) representing context information."
         (pb-prompt/update-focused-context
          (lambda (ctx) (pb-prompt/add-context-item ctx item))))

       (defun pb-prompt/add-path ()
         (interactive)
         (let ((path (read-file-name "Select directory or file: " nil nil t)))
           (when path
             (pb-prompt/add-item!
              (km :type (cond
                         ((file-directory-p path) "dir")
                         ((file-regular-p path) "file")
                         (t "unknown"))
                  :path path)))))

       (defun pb-prompt/add-project-file ()
         "Add a file from the current project to the prompt context.
          Uses projectile to select a file from the project."
         (interactive)
         (let ((project-root (projectile-project-root)))
           (when-let* ((file-path (projectile-completing-read
                                   "Add project file to context: "
                                   (projectile-project-files project-root))))
             (let ((full-path (concat project-root file-path)))
               (pb-prompt/add-item!
                (km :type "file"
                    :path full-path))
               (message "Added project file: %s" full-path)))))

       (defun pb-prompt/add-project-directory ()
         "Add a directory from the current project to the prompt context.
          Allows selecting a directory within the project structure."
         (interactive)
         (let* ((project-root (projectile-project-root))
                (project-dirs (cons project-root
                                    (seq-filter #'file-directory-p
                                                (directory-files-recursively
                                                 project-root "^[^.]" t))))
                (rel-dirs (mapcar (lambda (dir)
                                    (if (string= dir project-root)
                                        "/"
                                      (substring dir (length project-root))))
                                  project-dirs))
                (selected (completing-read "Add project directory to context: "
                                           rel-dirs nil t))
                (full-dir-path (if (string= selected "/")
                                   project-root
                                 (concat project-root selected))))
           (pb-prompt/add-item!
            (km :type "dir"
                :path full-dir-path))
           (message "Added project directory: %s" full-dir-path)))

       (defun pb-prompt/add-project-file-or-dir ()
         "Add a file or directory from the current project to the prompt context.
          Uses projectile to select a file from the project, or allows selecting
          a directory within the project structure."
         (interactive)
         (let ((choice (read-char-choice
                        "Select [f]ile or [d]irectory: "
                        '(?f ?d))))
           (cond
            ((eq choice ?f) (pb-prompt/add-project-file))
            ((eq choice ?d) (pb-prompt/add-project-directory)))))

       (defun pb-prompt/current-selection ()
         (interactive)
         (let ((selection
                (cond

                 ((eq major-mode 'org-mode)
                  (pb/let [(cons start end) (pb-org/node-bounds)]
                    (buffer-substring-no-properties start end)))

                 ;; If region is active, use region
                 ((use-region-p)
                  (buffer-substring-no-properties (region-beginning) (region-end)))

                 ;; If we're in pb-lisp mode, use current selection

                 ((and (boundp 'evil-state) (eq evil-state 'pb-lisp))
                  (pb-lisp/current-selection-as-string))

                 ;; If we're in symex-mode, use current symex
                 ((and (boundp 'symex-mode) symex-mode)
                  (pb-symex/current-as-string t))

                 ;; Default fallback message
                 (t
                  (user-error "No selection, pb-lisp selection, or symex available")))))
           (when selection
             (km :type "selection"
                 :path (buffer-file-name)
                 :major-mode major-mode
                 :symex symex-mode
                 :at (point)
                 :content selection))))

       (defun pb-prompt/add-selection ()
         "Add the current selection to the prompt context.
          Adds either the active region or current symex if in symex-mode."
         (interactive)
         (when-let ((selection (pb-prompt/current-selection)))
           (pb-prompt/add-item! selection)))

       (defun pb-prompt/add-buffer ()
         "Add the current buffer to the prompt context.
          This collects the buffer name, file path, major mode, and content
          for inclusion in the prompt context."
         (interactive)
         (pb-prompt/add-item!
          (km :type "buffer"
              :path (buffer-file-name)
              :buffer-name (buffer-name)
              :major-mode (symbol-name major-mode))))

       (defun pb-prompt/add-url (url &optional description)
         (interactive)
         (pb-prompt/add-item!
          (km :type "url"
              :url url
              :description description)))

       (defun pb-prompt/add-saved-context ()
         "Add a saved context to the current context.
          Prompts the user to select a saved context from the available options
          and adds it to the current context with a unique context-item ID."
         (interactive)
         (if-let* ((context-names (hash-table-keys pb-prompt/saved-contexts))
                   (selected-name (completing-read "Add saved context: " context-names nil t)))
             (let ((saved-context (gethash selected-name pb-prompt/saved-contexts)))
               (pb-prompt/add-item!
                (pb-prompt/with-id
                 (km :type "context"
                     :name selected-name)))
               (message "Added saved context '%s' to current context" selected-name))
           (message "No saved contexts available")))

       (defun pb-prompt/edit-function-in-buffer (func-data &optional callback)
         "Open a buffer to edit a function described in FUNC-DATA.

          FUNC-DATA should be a keyword map with at least:
          - :name - The name of the function (string)
          - Either :function (the actual function) or :code (string representation)

          Optional CALLBACK is called when the function is saved with C-c C-c.
          The callback receives the new function, the function expression string,
          and the potentially updated function name."
         (let* ((temp-buffer-name "*Function Editor*")
                (buffer (get-buffer-create temp-buffer-name))
                (func (km/get func-data :function))
                (func-name (km/get func-data :name))
                (func-str (if (and func (symbolp func))
                              (prin1-to-string (symbol-function func))
                            (km/get func-data :code))))
           (with-current-buffer buffer
             (erase-buffer)
             (emacs-lisp-mode)
             ;; Make the function name editable (wrapped in [[ ]] to indicate it's editable)
             (insert ";; Edit function: [[" func-name "]]\n")
             (insert ";; You can edit the name above (between [[ ]]) and the function body below\n")
             (insert ";; Press C-c C-c to save, C-c C-k to cancel\n\n")
             ;; Pretty print the function before inserting
             (let ((pp-func-str (with-temp-buffer
                                  (emacs-lisp-mode)
                                  (insert func-str)
                                  (goto-char (point-min))
                                  (indent-sexp)
                                  (buffer-string))))
               (insert pp-func-str))
             (goto-char (point-max))
             (backward-sexp)
             (indent-region (point-min) (point-max))
             (symex-mode-interface)
             (let ((map (make-sparse-keymap)))
               (define-key map (kbd "C-c C-c")
                           (lambda ()
                             (interactive)
                             (let* ((new-func-name
                                     (save-excursion
                                       (goto-char (point-min))
                                       (when (re-search-forward ";; Edit function: \\[\\[\\(.*?\\)\\]\\]" nil t)
                                         (match-string-no-properties 1))))
                                    (func-expr (buffer-substring-no-properties
                                                (save-excursion
                                                  (goto-char (point-min))
                                                  (search-forward "\n\n")
                                                  (point))
                                                (point-max)))
                                    (new-func (condition-case err
                                                  (eval (read func-expr))
                                                (error
                                                 (message "Error in function: %s" (error-message-string err))
                                                 nil))))
                               (if (functionp new-func)
                                   (progn
                                     (message "Function updated successfully")
                                     (when callback
                                       (funcall callback new-func func-expr new-func-name))
                                     (kill-buffer buffer))
                                 (message "Not a valid function")))))
               (define-key map (kbd "C-c C-k")
                           (lambda ()
                             (interactive)
                             (kill-buffer buffer)))
               (use-local-map (make-composed-keymap map (current-local-map))))
             ;; Position cursor at the start of the function name for convenience
             (goto-char (point-min))
             (re-search-forward ";; Edit function: \\[\\[" nil t)
             (message "Edit function name between [[ ]] and function body. Press C-c C-c when done"))
           (switch-to-buffer buffer)))

       (defun pb-prompt/add-function ()
         "Add a function (that generates context-item) to the context.
          This allows adding dynamic content to the prompt context that can be
          evaluated when needed during prompt generation.

          The user can either input a custom lambda function or select an existing
          function using completion."
         (interactive)
         (let* ((choice (read-char-choice
                         "Select function method [e]xisting or [c]ustom lambda: "
                         '(?e ?c))))

           (cond
            ;; Select existing function via completion
            ((eq choice ?e)
             (let* ((obarray-functions
                     (seq-filter (lambda (sym)
                                   (and (symbolp sym)
                                        (fboundp sym)
                                        (not (special-form-p sym))
                                        (not (macrop sym))))
                                 obarray))
                    (selected-func-sym (intern (completing-read
                                                "Select function: "
                                                obarray-functions
                                                #'fboundp t))))
               (pb-prompt/add-item!
                (km :type "function"
                    :name (symbol-name selected-func-sym)
                    :function (symbol-function selected-func-sym)
                    :documentation (or (documentation selected-func-sym) "No documentation available")))))

            ;; Custom lambda input
            ((eq choice ?c)
             (let ((func-name (read-string "Function name: ")))
               (pb-prompt/edit-function-in-buffer
                (km :name func-name
                    :code "(lambda ()\n (interactive)\n ())")
                (lambda (func func-expr new-func-name)
                  (pb-prompt/add-item!
                   (km :type "function"
                       :name (or new-func-name func-name)
                       :code func-expr
                       :function func
                       :documentation (or (documentation func) "No documentation available")))))))))))

(progn :request

       (defun pb-prompt/simple-request ()
         (interactive)
         (gptel-request
             (pb-prompt/mk (km :instructions
                               (km :base "You are a useful code assistant, you really like lisp-like languages and you know how to balance parentheses correctly."
                                   :response-format ["Your response should be valid code, intended to replace the current expression in a source code file."
                                                     "Don't use markdown code block syntax or any non-valid code in your output."
                                                     "If you have to write prose, use appropriate comment syntax."]
                                   :selection (pb-prompt/current-selection)
                                   :task (read-string "Edit current expression: "))))

           :system (pb-prompt/context-prompt)
           :callback #'pb-gptel/current-symex-request-handler))

       (defun pb-prompt/buffer-request-handler (res info)
         (if res
             (progn
               (cond ((eq major-mode 'org-mode) (pb-org/delete))
                     (symex-mode (symex-change 1)))
               (insert res)
               (cond ((eq major-mode 'org-mode)
                      (insert "\n\n")
                      (evil-sorg-state 1))
                     (symex-mode
                      (symex-mode-interface)
                      (symex-tidy))))
           (message (km/pp (km :status (km/get info :http-status)
                               :error (km/get info :error)))))
         (setq evil-sorg-state-cursor `("gold" box) )
         (setq evil-symex-state-cursor `("cyan" box) )
         (evil-refresh-cursor))

       (defun pb-prompt/buffer-request-base-instruction ()
         (cond ((eq 'org-mode major-mode)
                (km :base "You are editing an org-mode buffer, you are very aware its tree structure."
                    :response-format ["Your response should be valid org content, intended to replace the current node or subtree in the org file."
                                      "Don't use markdown code block syntax or any non-valid org in your output."]))
               (symex-mode
                (km :base "You are a useful code assistant, you really like lisp-like languages and you know how to balance parentheses correctly."
                    :response-format ["Your response should be valid code, intended to replace the current expression in a source code file."
                                      "Don't use markdown code block syntax or any non-valid code in your output."
                                      "If you have to write prose, use appropriate comment syntax."]))))

       (defun pb-prompt/buffer-request (&optional instruction)
         "Send a GPT request with the current buffer context.
          In Org mode, uses the current Org node as selection.
          Otherwise uses current selection or expression at point."
         (interactive)
         (let ((selection (pb-prompt/current-selection)))
           (gptel-request
               (pb-prompt/mk (km :instructions
                                 (km/merge
                                  (pb-prompt/buffer-request-base-instruction)
                                  (km :buffer-info (km :name (buffer-name)
                                                       :file-path (buffer-file-name)
                                                       :major-mode (symbol-name major-mode)
                                                       :point (point)
                                                       :line-number (line-number-at-pos))
                                      :buffer-content (buffer-substring-no-properties (point-min) (point-max))
                                      :selection selection
                                      :task (or instruction
                                                (read-string "Edit current selection: "))))))

             :system (pb-prompt/context-prompt)
             :callback #'pb-prompt/buffer-request-handler)

           (let ((start (km/get selection :at))
                 (end (+ (km/get selection :at)
                         (length (km/get selection :content)))))
             (symex--delete-overlay)
             (let ((overlay (make-overlay start end)))
               (overlay-put overlay 'face (km :background pb-gptel/overlay-color))))

           (run-at-time 0.1 nil
                        (lambda ()
                          (cond ((and (eq major-mode 'org-mode)
                                      (not (evil-sorg-state-p)))
                                 (evil-sorg-state 1))
                                (symex-mode
                                 (symex-mode-interface)))
                          (setq evil-sorg-state-cursor `("green" box) )
                          (setq evil-symex-state-cursor `("green" box) )
                          (evil-refresh-cursor))))))

(progn :item-navigation

       (defun pb-prompt/goto-next-item ()
         "Move to the next item in the buffer."
         (interactive)
         (let ((orig-point (point))
               (found nil))
           (forward-line 1)
           (while (and (not (eobp))
                       (not found))
             (beginning-of-line)
             (if (looking-at "^•")
                 (setq found t)
               (forward-line 1)))
           (when (not found)
             (goto-char orig-point)
             (message "No next context found"))))

       (defun pb-prompt/goto-previous-item ()
         "Move to the previous item in the buffer."
         (interactive)
         (let ((orig-point (point))
               (found nil))
           (forward-line -1)
           (while (and (not (bobp))
                       (not found))
             (beginning-of-line)
             (if (looking-at "^•")
                 (setq found t)
               (forward-line -1)))
           (when (not found)
             (goto-char orig-point)
             (message "No previous context found"))))

       (defun pb-prompt/focus-item-by-name (name)
         "Focus item NAME in saved context special buffer.
          Find the context with NAME in the saved contexts buffer and move point to it.
          This function is useful for navigating to a specific context in the
          browser view after operations that might change the buffer content."
         (when (and name
                    (eq major-mode 'pb-prompt/saved-contexts-mode))
           (let ((orig-point (point))
                 (found nil))
             (save-excursion
               (goto-char (point-min))
               (while (and (not found)
                           (re-search-forward (concat "• " (regexp-quote name)) nil t))
                 (beginning-of-line)
                 (when (looking-at (concat "• " (regexp-quote name)))
                   (setq found (point)))))
             (if found
                 (progn
                   (goto-char found)
                   (recenter)
                   t)
               (goto-char orig-point)
               (message "Context '%s' not found in buffer" name)
               nil)))))

(progn :bindings

       (defvar pb-prompt/context-browser-keybindings
         '((:key "RET" :desc "Browse item at point"
            :category "navigation"
            :function pb-prompt/browse-context-item-at-point)
           (:key "d" :desc "Delete item at point"
            :category "edition"
            :function pb-prompt/delete-context-item-at-point)
           (:key "v" :desc "View item details"
            :category "display"
            :function pb-prompt/view-context-item-at-point)
           (:key "r" :desc "Refresh browser"
            :category "display"
            :function pb-prompt/refresh-context-browser)
           (:key "a f" :desc "Add file/path to context"
            :category "edition"
            :function pb-prompt/add-path-to-focused-context)
           (:key "a p" :desc "Add project file or dir to context"
            :category "edition"
            :function pb-prompt/add-project-file-or-dir-to-focused-context)
           (:key "q" :desc "Quit window"
            :category "navigation"
            :function quit-window)
           (:key "k" :desc "Go to parent context"
            :category "navigation"
            :function pb-prompt/browse-parent-context)
           (:key "l" :desc "Go to next item"
            :category "navigation"
            :function pb-prompt/goto-next-item)
           (:key "h" :desc "Go to previous item"
            :category "navigation"
            :function pb-prompt/goto-previous-item)
           (:key "j" :desc "Browse child context"
            :category "navigation"
            :function pb-prompt/browse-child-context)
           (:key "y" :desc "Copy item at point"
            :category "clipboard"
            :function pb-prompt/copy-context-item-at-point)
           (:key "p" :desc "Yank copied item"
            :category "clipboard"
            :function pb-prompt/yank-context-item)
           (:key "C-p" :desc "Yank item from ring"
            :category "clipboard"
            :function pb-prompt/yank-context-item-from-ring)
           (:key "x" :desc "Toggle disable item at point"
            :category "edition"
            :function pb-prompt/toggle-context-item-disabled-at-point)
           (:key "?" :desc "Show help menu"
            :category "help"
            :function pb-prompt/context-browser-help-menu)))

       (defvar pb-prompt/saved-contexts-keybindings
         '((:key "RET" :desc "Browse context at point"
            :category "navigation"
            :function pb-prompt/browse-saved-context)
           (:key "d" :desc "Delete context at point"
            :category "edition"
            :function pb-prompt/delete-context-at-point)
           (:key "l" :desc "Load context at point"
            :category "edition"
            :function pb-prompt/load-context-at-point)
           (:key "a" :desc "Append context at point"
            :category "edition"
            :function pb-prompt/append-context-at-point)
           (:key "q" :desc "Quit window"
            :category "navigation"
            :function pb-prompt/exit-saved-contexts)
           (:key "k" :desc "Go to parent/quit"
            :category "navigation"
            :function pb-prompt/exit-saved-contexts)
           (:key "l" :desc "Go to next item"
            :category "navigation"
            :function pb-prompt/goto-next-item)
           (:key "h" :desc "Go to previous item"
            :category "navigation"
            :function pb-prompt/goto-previous-item)
           (:key "j" :desc "Browse context"
            :category "navigation"
            :function pb-prompt/browse-saved-context)
           (:key "v" :desc "View context details"
            :category "display"
            :function pb-prompt/open-context-at-point)
           (:key "y" :desc "Copy context at point"
            :category "clipboard"
            :function pb-prompt/copy-context-at-point)
           (:key "?" :desc "Show help menu"
            :category "help"
            :function pb-prompt/saved-contexts-help-menu))))

(progn :help-menu

       (defun pb-prompt/display-help-menu (title bindings-var)
         "Display available keybindings in a help window with proper formatting,
          grouped by category.

          TITLE is the header title for the help buffer.
          BINDINGS-VAR is a variable containing the keybindings to display."
         (let* ((bindings (symbol-value bindings-var))
                (categories (delete-dups (mapcar (lambda (b) (km/get b :category)) bindings)))
                (sorted-categories (sort categories #'string<))
                (buf-name (format "*%s Help*" title))
                (buf (get-buffer-create buf-name)))
           (with-current-buffer buf
             (let ((inhibit-read-only t))
               (erase-buffer)
               (insert (propertize (concat title " Keybindings:\n") 'face 'bold))
               (insert (propertize "==========================\n\n" 'face 'bold))

               ;; Display bindings grouped by category
               (dolist (category sorted-categories)
                 ;; Category header
                 (insert (propertize (concat (capitalize category) ":\n")
                                     'face '(:inherit font-lock-type-face :weight bold)))
                 (insert (propertize (make-string (+ (length category) 1) ?-)
                                     'face 'font-lock-comment-face)
                         "\n")

                 ;; Get bindings for this category
                 (let* ((category-bindings (seq-filter
                                            (lambda (b) (string= (km/get b :category) category))
                                            bindings))
                        (max-key-length (apply #'max
                                               (mapcar (lambda (b) (length (km/get b :key)))
                                                       category-bindings)))
                        (max-desc-length (apply #'max
                                                (mapcar (lambda (b) (length (km/get b :desc)))
                                                        category-bindings))))

                   ;; Sort bindings within category by key
                   (dolist (binding (sort category-bindings
                                          (lambda (a b)
                                            (string< (km/get a :key) (km/get b :key)))))
                     (let* ((key (km/get binding :key))
                            (desc (km/get binding :desc))
                            (func (km/get binding :function))
                            (key-padding (make-string (- max-key-length (length key)) ? ))
                            (desc-padding (make-string (- max-desc-length (length desc)) ? )))
                       (insert "  " (propertize key 'face 'font-lock-keyword-face))
                       (insert key-padding "  ")
                       (insert (propertize desc 'face 'font-lock-doc-face))
                       (insert desc-padding "  → ")
                       (insert (propertize (symbol-name func) 'face 'font-lock-function-name-face))
                       (insert "\n")))
                   (insert "\n")))

               (help-mode)
               (goto-char (point-min))))
           (pop-to-buffer buf)))

       (defun pb-prompt/context-browser-help-menu ()
         "Display help for context browser keybindings."
         (interactive)
         (pb-prompt/display-help-menu "Context Browser" 'pb-prompt/context-browser-keybindings))

       (defun pb-prompt/saved-contexts-help-menu ()
         "Display help for saved contexts keybindings."
         (interactive)
         (pb-prompt/display-help-menu "Saved Contexts" 'pb-prompt/saved-contexts-keybindings)))

(progn :context-browser

       (defvar-local pb-prompt/parent-context ())

       (defvar pb-prompt/context-ring ())

       (defvar pb-prompt/context-browser-mode-map
         (make-sparse-keymap)
         "Keymap for `pb-prompt/context-browser-mode'.")

       (define-derived-mode pb-prompt/context-browser-mode
         special-mode
         "Context Browser"
         "Major mode for browsing context items.
          \\{pb-prompt/context-browser-mode-map}"
         (setq buffer-read-only t))

       (with-eval-after-load 'evil
         (evil-set-initial-state 'pb-prompt/context-browser-mode 'normal)
         (dolist (binding pb-prompt/context-browser-keybindings)
           (evil-define-key 'normal pb-prompt/context-browser-mode-map
             (kbd (km/get binding :key)) (km/get binding :function))))

       (defun pb-prompt/browse-context (&optional name)
         "Browse items in CONTEXT in a dedicated buffer.
          If CONTEXT is nil, use =pb-prompt/context=.
          Optional NAME is used for the buffer title."
         (interactive)
         (let* ((context (cond ((string= "current" name) pb-prompt/context)
                               (name (gethash name pb-prompt/saved-contexts))
                               (t pb-prompt/context)))
                (buffer (get-buffer-create (concat "*Context Browser " (or name "current") "*")))
                (inhibit-read-only t))
           (with-current-buffer buffer
             (erase-buffer)
             (if (null context)
                 (insert (propertize "No context items found.\n" 'face 'font-lock-comment-face))
               (let* ((title (if name
                                 (concat "* " name)
                               "Current Context"))
                      (items-by-type (seq-group-by
                                      (lambda (item) (km/get item :type))
                                      context)))

                 (insert (propertize "RET: browse, h.j.k.l: navigate, ?: help, q: quit\n"
                                     'face 'lsp-details-face))

                 ;; Insert title with face
                 (insert "\n" (propertize title 'face 'custom-modified) "\n\n\n")
                 '(insert (propertize (make-string (length title) ?-) 'face 'font-lock-comment-face) "\n\n")

                 ;; Group by type
                 (dolist (type-group (sort items-by-type
                                           (lambda (a b) (string< (car a) (car b)))))
                   (let ((type (car type-group))
                         (items (cdr type-group)))
                     ;; Type header with face
                     (insert (concat (propertize ": " 'face 'font-lock-doc-face)
                                     (propertize type 'face 'magit-diff-removed)
                                     (propertize (format " (%d)" (length items)) 'face 'font-lock-doc-face))
                             "\n\n")

                     ;; List items of this type
                     (dolist (item items)
                       (let* ((id (km/get item :id))
                              (desc (pb-prompt/-context-item-description item))
                              (disabled (km/get item :disabled))
                              (bullet (propertize "• " 'face 'custom-set))
                              (id-text (propertize (format "[%s]" id) 'face 'lsp-details-face))
                              (line (format "%s%s %s\n" bullet desc id-text)))
                         (add-text-properties 0 (length line)
                                              `(context-item ,item context-item-id ,id) line)
                         (if disabled
                             (insert (propertize line 'face 'font-lock-comment-face))
                           (insert line))))
                     (insert "\n")))))

             (goto-char (point-min))
             ;; Set mode and header line
             (pb-prompt/context-browser-mode)
             (setq-local pb-prompt/context-browser-focus name)
             '(setq-local header-line-format
                          (propertize "RET: browse, d: delete, v: view details, r: refresh, q: quit"
                                      'face 'font-lock-comment-face)))

           (switch-to-buffer buffer)
           (pb-prompt/goto-next-item)))

       (defun pb-prompt/refresh-context-browser ()
         "Refresh the context browser buffer."
         (interactive)
         (when (eq major-mode 'pb-prompt/context-browser-mode)
           (let ((line (line-number-at-pos)))
             (pb-prompt/browse-context pb-prompt/context-browser-focus)
             (goto-char (point-min))
             (forward-line (1- line)))))

       (progn :browse

              (defun pb-prompt/browse-current-context ()
                "Browse the current context in a dedicated buffer."
                (interactive)
                (pb-prompt/browse-context))

              (defun pb-prompt/browse-parent-context ()
                "Navigate to the parent of the current context browser.

                 If the buffer-local variable `pb-prompt/parent-context' is bound,
                 navigate to that parent context in the browser. Otherwise, if there
                 is no parent context but the current context is a saved context
                 (indicated by `pb-prompt/context-browser-focus' being non-nil),
                 go back to the full list of saved contexts.

                 This function provides hierarchical navigation in the context browser,
                 allowing users to move up the context tree during exploration."
                (interactive)
                (cond (pb-prompt/parent-context
                       (pb-prompt/browse-context pb-prompt/parent-context))
                      (pb-prompt/context-browser-focus
                       (pb-prompt/browse-saved-contexts pb-prompt/context-browser-focus))))

              (defun pb-prompt/browse-child-context ()
                "If item at point is a context, open a Context Browser for it.
                 Navigates to the child context and sets the parent context locally
                 to enable navigation back to the current context."
                (interactive)
                (when-let* ((item (pb-prompt/context-item-at-point))
                            (type (km/get item :type))
                            (name (km/get item :name)))
                  (if (equal type "context")
                      (let ((parent-name (or pb-prompt/context-browser-focus "current"))
                            (child-buffer (concat "*Context Browser " name "*")))
                        (pb-prompt/browse-context name)
                        (with-current-buffer child-buffer
                          (setq-local pb-prompt/parent-context parent-name)
                          (message "Navigated to child context: %s (parent: %s)"
                                   name parent-name)))
                    (message "Item at point is not a context (type: %s)" type))))

              (defun pb-prompt/back-to-saved-contexts ()
                "Return to the saved contexts listing view from the context browser.
                 This function closes the current context browser and opens the saved contexts list."
                (interactive)
                (when (eq major-mode 'pb-prompt/context-browser-mode)
                  (let ((buffer (current-buffer))
                        (current-context-name pb-prompt/context-browser-focus))
                    (quit-window)
                    (pb-prompt/browse-saved-contexts)
                    (when current-context-name
                      (with-current-buffer pb-prompt/saved-context-buffer-name
                        (print current-context-name)
                        (print (current-buffer))
                        (goto-char (point-min))
                        (when (search-forward current-context-name nil t)
                          (beginning-of-line))))))))

       (progn :selection

              (defun pb-prompt/-format-relative-path (path)
                "Format PATH as a relative path to project root or home directory.
                 If PATH is in a project, return it relative to the project root.
                 If PATH is in the home directory, return it with ~ prefix.
                 Otherwise, return the expanded path."
                (let* ((project-root (when (and (fboundp 'project-current)
                                                (project-current))
                                       (project-root (project-current))))
                       (expanded-path (expand-file-name path))
                       (relative-path (cond
                                       ((and project-root
                                             (string-prefix-p project-root expanded-path))
                                        (substring expanded-path (length project-root)))
                                       ((string-prefix-p (expand-file-name "~") expanded-path)
                                        (concat "~" (substring expanded-path (length (expand-file-name "~")))))
                                       (t expanded-path))))
                  relative-path))

              (defun pb-prompt/-context-item-description (item)
                "Format a description for a context ITEM based on its type.
                 Returns a human-readable string representation of the item.

                 For buffer items, returns the buffer name.
                 For file or directory items, returns the relative path.
                 For selection items, returns the first line (truncated if needed).
                 For other types, returns a generic description with the type name."
                (let ((type (km/get item :type))
                      (path (km/get item :path)))
                  (cl-case (intern type)
                    (buffer
                     (km/get item :buffer-name))
                    (context
                     (km/get item :name))
                    ((file dir)
                     (pb-prompt/-format-relative-path path))
                    (selection
                     (let* ((content (km/get item :content))
                            (first-line (car (split-string content "\n")))
                            (truncated-line (truncate-string-to-width first-line 80 nil nil "...")))
                       truncated-line))
                    (function
                     (let* ((content (km/get item :documentation))
                            (first-line (car (split-string content "\n")))
                            (truncated-line (truncate-string-to-width first-line 80 nil nil "...")))
                       (concat (km/get item :name) " :: " (propertize truncated-line
                                                                      'face 'term-faint))))
                    (otherwise
                     (format "Item of type: %s" type)))))

              (defun pb-prompt/select-context-item ()
                (interactive)
                (when pb-prompt/context
                  (let* ((items-by-type (seq-group-by
                                         (lambda (item) (km/get item :type))
                                         pb-prompt/context))
                         ;; Create a mapping from display strings to actual items
                         (item-map (make-hash-table :test 'equal))
                         ;; Create candidates with proper properties for grouping
                         (candidates (mapcan
                                      (lambda (type-group)
                                        (let ((type (car type-group))
                                              (items (cdr type-group)))
                                          (mapcar
                                           (lambda (item)
                                             (let* ((desc (pb-prompt/-context-item-description item)))
                                               ;; Store in our map for later retrieval
                                               (puthash desc item item-map)
                                               ;; Return the display candidate with item properties
                                               (propertize desc
                                                           'consult--group type
                                                           'context-item item)))
                                           items)))
                                      items-by-type))
                         ;; Define group function for consult
                         (group-function (lambda (cand transform)
                                           (if transform
                                               cand
                                             (get-text-property 0 'consult--group cand))))
                         ;; Get selections using consult--read
                         (picked (consult--read candidates
                                                :prompt "Select context item: "
                                                :category 'pb-prompt-context-item
                                                :group group-function
                                                :require-match t
                                                :sort nil)))

                    (gethash picked item-map))))

              (defun pb-prompt/consult-context ()
                (interactive)
                (pb-prompt/browse-context-item))

              ;; Define embark actions for context items
              (with-eval-after-load 'embark

                (defun pb-prompt/embark-browse-context-item (candidate)
                  (if-let ((item (get-text-property 0 'context-item candidate)))
                      (pb-prompt/browse-context-item item)))

                (defun pb-prompt/embark-describe-context-item (candidate)
                  "Display detailed information about the context item from embark."
                  (if-let ((item (get-text-property 0 'context-item candidate)))
                      (pb-prompt/describe-context-item item)
                    (message "No context item found in candidate")))

                (defun pb-prompt/embark-remove-context-item (candidate)
                  "Remove the context item from embark candidate."
                  (if-let ((item (get-text-property 0 'context-item candidate)))
                      (pb-prompt/remove-context-item item)
                    (message "No context item found in candidate")))

                (setq embark-quit-after-action
                      '((pb-prompt/embark-browse-context-item . t)
                        (pb-prompt/embark-describe-context-item . nil)
                        (pb-prompt/embark-remove-context-item . nil)
                        (t . t)))

                (add-to-list 'embark-keymap-alist
                             '(pb-prompt-context-item . pb-prompt-context-item-map))

                (defvar pb-prompt-context-item-map
                  (make-sparse-keymap))

                (map!
                 (:map pb-prompt-context-item-map
                  :desc "Browse item" "b" #'pb-prompt/embark-browse-context-item
                  :desc "Describe item details" "d" #'pb-prompt/embark-describe-context-item
                  :desc "Remove item" "r" #'pb-prompt/embark-remove-context-item
                  :desc "Kill/delete item" "k" #'pb-prompt/embark-remove-context-item))))

       (progn :actions

              (defun pb-prompt/remove-context-item (&optional item)
                "Let the user interactively remove some items from pb-prompt/context.
                 The items are grouped by type in the completion UI via category property."
                (interactive)
                (if-let ((to-remove (or item (pb-prompt/select-context-item))))

                    (progn (message "Removing context item: %s (ID: %s)"
                                    (or (km/get to-remove :type) "unknown")
                                    (km/get to-remove :id))
                           (setq pb-prompt/context
                                 (seq-remove (lambda (item)
                                               (equal (km/get item :id) (km/get to-remove :id)))
                                             pb-prompt/context)))

                  (message "Warning: No item selected for removal (Key: %s)"
                           picked)))

              (defun pb-prompt/browse-context-item (&optional item)
                "Open or display the content of the selected context item."
                (interactive)
                (let* ((item (or item (pb-prompt/select-context-item)))
                       (type (km/get item :type)))
                  (cl-case (intern type)
                    (buffer
                     (switch-to-buffer (km/get item :buffer-name)))
                    (context
                     (pb-prompt/browse-context
                      (km/get item :name)))
                    (file
                     (find-file-other-window (km/get item :path)))
                    (dir
                     (dired-other-window (km/get item :path)))
                    (selection
                     (with-current-buffer (get-buffer-create "*Context Item Selection*")
                       (erase-buffer)
                       (insert (km/get item :content))
                       (goto-char (point-min))
                       (when-let ((path (km/get item :path))
                                  (mode (km/get item :major-mode)))
                         (funcall mode)
                         (when (km/get item :symex)
                           (symex-mode-interface)
                           (symex-tidy)))
                       (pop-to-buffer (current-buffer))))
                    (function
                     (pb-prompt/browse-function-item item))
                    (otherwise
                     (message "Don't know how to browse item of type: %s" type)))))

              (defun pb-prompt/browse-function-item (item)
                "Open a buffer to edit the function in ITEM.
                 The function is extracted from the item and displayed in a buffer.
                 User can edit the function and save or cancel their changes."
                (pb-prompt/edit-function-in-buffer
                 item
                 (lambda (new-func func-expr new-func-name)
                   (pb-prompt/update-focused-context
                    (lambda (ctx)
                      (mapcar (lambda (ctx-item)
                                (if (equal (km/get ctx-item :id) (km/get item :id))
                                    (km/put ctx-item
                                            :function new-func
                                            :code func-expr
                                            :documentation (or (documentation new-func) "No documentation available")
                                            :name (or new-func-name (km/get ctx-item :name)))
                                  ctx-item))
                              ctx))))))

              (defun pb-prompt/describe-context-item (&optional item)
                "Show detailed information about the context item."
                (with-current-buffer (get-buffer-create "*Context Item Details*")
                  (erase-buffer)
                  (let* ((item (or item (pb-prompt/select-context-item)))
                         (id (km/get item :id))
                         (type (km/get item :type))
                         (path (km/get item :path)))
                    (insert (format "ID: %s\n" id)
                            (format "Type: %s\n" type))
                    (when path
                      (insert (format "Path: %s\n" path)))

                    (dolist (key (mapcar #'car (km/entries item)))
                      (unless (member key '(:id :type :path :content))
                        (insert (format "%s: %s\n"
                                        (substring (symbol-name key) 1)
                                        (km/get item key)))))

                    (when-let ((content (km/get item :content)))
                      (insert "\nContent:\n---------\n")
                      (insert content)))
                  (goto-char (point-min))
                  (pop-to-buffer (current-buffer)))))

       (progn :at-point

              (defun pb-prompt/context-item-at-point ()
                "Get the context item at point in the browser buffer."
                (get-text-property (point) 'context-item))

              (defun pb-prompt/browse-context-item-at-point ()
                "Browse the context item at point."
                (interactive)
                (when-let ((item (pb-prompt/context-item-at-point)))
                  (pb-prompt/browse-context-item item)))

              (defun pb-prompt/copy-context-item-at-point ()
                (interactive)
                (when-let ((item (pb-prompt/context-item-at-point)))
                  (add-to-list 'pb-prompt/context-ring item)))

              (defun pb-prompt/yank-context-item ()
                (interactive)
                (pb-prompt/add-item! (car pb-prompt/context-ring))
                (pb-prompt/refresh-context-browser))

              (defun pb-prompt/yank-context-item-from-ring ()
                "Prompt the user to select a context-item from `pb-prompt/context-ring` and yank it to the current context.
                 This lets you reuse items previously copied from the browser."
                (interactive)
                (when pb-prompt/context-ring
                  (let* ((candidates (mapcar
                                      (lambda (item)
                                        (let ((desc (pb-prompt/-context-item-description item)))
                                          (cons desc item)))
                                      pb-prompt/context-ring))
                         (desc-list (mapcar #'car candidates))
                         (picked-desc (completing-read "Yank context item: " desc-list nil t))
                         (item (cdr (assoc picked-desc candidates))))
                    (when item
                      (pb-prompt/add-item! item)
                      (pb-prompt/refresh-context-browser)
                      (message "Yanked context item: %s" picked-desc)))))

              (defun pb-prompt/view-context-item-at-point ()
                "View detailed information about the context item at point."
                (interactive)
                (when-let ((item (pb-prompt/context-item-at-point)))
                  (pb-prompt/describe-context-item item)))

              (defun pb-prompt/add-path-to-focused-context ()
                (interactive)
                (pb-prompt/add-path)
                (pb-prompt/refresh-context-browser))

              (defun pb-prompt/add-project-file-or-dir-to-focused-context ()
                (interactive)
                (pb-prompt/add-project-file-or-dir)
                (pb-prompt/refresh-context-browser))

              (defun pb-prompt/toggle-context-item-disabled-at-point ()
                "Toggle the :disabled property of the context item at point."
                (interactive)
                (print "toggle disabled")
                (when-let ((item (pb-prompt/context-item-at-point)))
                  (pb-prompt/update-focused-context
                   (lambda (ctx)
                     (mapcar (lambda (ctx-item)
                               (if (equal (km/get ctx-item :id) (km/get item :id))
                                   (km/put ctx-item :disabled (not (km/get ctx-item :disabled)))
                                 ctx-item))
                             ctx)))
                  (pb-prompt/refresh-context-browser)
                  (message "Toggled disabled for item: %s" (pb-prompt/-context-item-description item))))

              (defun pb-prompt/delete-context-item-at-point ()
                "Delete the context item at point from the current context."
                (interactive)
                (when-let* ((item (pb-prompt/context-item-at-point))
                            (id (km/get item :id)))
                  (if (yes-or-no-p (format "Delete item %s? "
                                           (pb-prompt/-context-item-description item)))
                      (progn
                        (add-to-list 'pb-prompt/context-ring item)
                        (pb-prompt/update-focused-context
                         (lambda (ctx)
                           (seq-remove (lambda (i)
                                         (equal (km/get i :id) id))
                                       ctx)))
                        (pb-prompt/refresh-context-browser)
                        (message "Item deleted"))
                    (message "Deletion cancelled"))))))

(progn :saved-contexts

       (defvar pb-prompt/saved-context-buffer-name
         "*Saved Contexts*")

       (define-derived-mode pb-prompt/saved-contexts-mode
         special-mode
         "PB-Prompt Contexts"
         "Major mode for listing saved prompt contexts."
         (setq buffer-read-only t))

       (defvar pb-prompt/saved-contexts-mode-map
         (make-sparse-keymap)
         "Keymap for `pb-prompt/saved-contexts-mode'.")

       (with-eval-after-load 'evil
         (evil-set-initial-state 'pb-prompt/saved-contexts-mode 'normal)
         (dolist (binding pb-prompt/saved-contexts-keybindings)
           (evil-define-key 'normal pb-prompt/saved-contexts-mode-map
             (kbd (km/get binding :key)) (km/get binding :function))))

       (defun pb-prompt/browse-saved-contexts (&optional focus-name)
         "Show a list of all saved contexts with item counts.
          Displays contexts with syntax highlighting for better readability."
         (interactive)

         (with-current-buffer (get-buffer-create pb-prompt/saved-context-buffer-name)
           (let ((contexts (hash-table-keys pb-prompt/saved-contexts))
                 (inhibit-read-only t))
             (erase-buffer)
             (if (null contexts)
                 (insert (propertize "No saved contexts found.\n" 'face 'font-lock-comment-face))
               ;; Header with custom face
               (insert (propertize "Saved Contexts:\n\n" 'face 'font-lock-keyword-face))
               (dolist (name (sort contexts #'string<))
                 (let* ((context (gethash name pb-prompt/saved-contexts))
                        (count (length context))
                        (types (mapcar (lambda (item) (pb/keyword (km/get item :type))) context))
                        (type-counts (seq-reduce
                                      (lambda (acc type)
                                        (km/upd acc type (pb/fn [c] (1+ (or c 0)))))
                                      types
                                      nil)))
                   ;; Context name with bullet and count
                   (insert (propertize "• " 'face '(:foreground "pink"))
                           name
                           "\n")

                   ;; Types section with different face
                   (let ((type-strs
                          (mapcar (pb/fn [(cons type count)]
                                         (concat (propertize (pb/string type) 'face 'font-lock-doc-face)
                                                 (propertize (format " %d" count)
                                                             'face 'font-lock-type-face)))
                                  (km/entries type-counts))))
                     (insert "  ")
                     (insert (mapconcat #'identity type-strs ", "))
                     (insert "\n\n"))))))

           (goto-char (point-min))
           (pb-prompt/saved-contexts-mode)
           (setq-local header-line-format
                       (propertize "RET: open, d: delete, l: load, a: append, ?: help, q: quit"
                                   'face 'header-line))
           (switch-to-buffer (current-buffer))
           (if focus-name
               (pb-prompt/focus-item-by-name focus-name)
             (pb-prompt/goto-next-item))))

       (defun pb-prompt/exit-saved-contexts ()
         "Exit the saved contexts buffer, kill the buffer, and quit the window."
         (interactive)
         (let ((buffer (current-buffer)))
           (quit-window t)))

       (progn :context-actions

              (defun pb-prompt/get-saved-context (name)
                (gethash name pb-prompt/saved-contexts))

              (defun pb-prompt/save-context (name)
                "Save the current context with NAME for later retrieval.
                 If a context with NAME already exists, it will be overwritten after confirmation."
                (interactive "sSave context as: ")
                (when (and (gethash name pb-prompt/saved-contexts)
                           (not (yes-or-no-p (format "Context '%s' already exists. Overwrite? " name))))
                  (user-error "Aborted"))

                (puthash name
                         (copy-tree pb-prompt/context)
                         pb-prompt/saved-contexts)

                (pb-prompt/save-contexts-to-file)

                (message "Context saved as '%s'" name))

              (defun pb-prompt/load-context (name)
                "Load a previously saved context with NAME.
                 If the current context is not empty, it will be replaced after confirmation."
                (interactive
                 (list (completing-read "Load context: "
                                        (hash-table-keys pb-prompt/saved-contexts) nil t)))
                (let ((saved-context (gethash name pb-prompt/saved-contexts)))
                  (unless saved-context
                    (user-error "No context found with name '%s'" name))

                  (when (and pb-prompt/context
                             (not (yes-or-no-p "Replace current context? ")))
                    (user-error "Aborted"))

                  (setq pb-prompt/context (copy-tree saved-context))
                  (message "Loaded context '%s' with %d items"
                           name (length pb-prompt/context))))

              (defun pb-prompt/append-context (name)
                "Append items from a saved context with NAME to the current context."
                (interactive
                 (list (completing-read "Append context: "
                                        (hash-table-keys pb-prompt/saved-contexts) nil t)))
                (let ((saved-context (gethash name pb-prompt/saved-contexts)))
                  (unless saved-context
                    (user-error "No context found with name '%s'" name))

                  (setq pb-prompt/context
                        (append pb-prompt/context (copy-sequence saved-context)))
                  (message "Appended %d items from context '%s'"
                           (length saved-context) name)))

              (defun pb-prompt/delete-saved-context (name)
                "Delete a saved context with NAME."
                (interactive
                 (list (completing-read "Delete context: "
                                        (hash-table-keys pb-prompt/saved-contexts) nil t)))
                (if (gethash name pb-prompt/saved-contexts)
                    (progn
                      (remhash name pb-prompt/saved-contexts)
                      (message "Deleted context '%s'" name))
                  (message "No context found with name '%s'" name)))

              (defun pb-prompt/browse-saved-context ()
                "Browse the context at point in saved contexts buffer."
                (interactive)
                (when-let* ((name (pb-prompt/context-name-at-point))
                            (trimmed-name (string-trim name))
                            (context (gethash trimmed-name pb-prompt/saved-contexts)))
                  (pb-prompt/browse-context trimmed-name))))

       (progn :at-point

              (defun pb-prompt/context-name-at-point ()
                "Get context name at point in the saved contexts buffer."
                (save-excursion
                  (beginning-of-line)
                  (when (looking-at "• \\(.+\\)")
                    (match-string-no-properties 1))))

              (defun pb-prompt/open-context-at-point ()
                "View the context at point in detail."
                (interactive)
                (let ((name (pb-prompt/context-name-at-point)))
                  (when name
                    (let ((context (gethash (string-trim name) pb-prompt/saved-contexts)))
                      (with-current-buffer (let ((buf (get-buffer "*Context Details*")))
                                             (when buf (kill-buffer buf))
                                             (get-buffer-create "*Context Details*"))
                        (erase-buffer)
                        (insert (format "Context: %s (%d items)\n\n"
                                        (string-trim name) (length context)))
                        (dolist (item context)
                          (insert (format "Type: %s\n" (km/get item :type)))
                          (when-let ((id (km/get item :id)))
                            (insert (format "ID: %s\n" id)))
                          (when-let ((path (km/get item :path)))
                            (insert (format "Path: %s\n" path)))
                          (let ((content (km/get item :content)))
                            (when content
                              (insert "Content preview: ")
                              (let ((preview (if (> (length content) 100)
                                                 (concat (substring content 0 100) "...")
                                               content)))
                                (insert (replace-regexp-in-string "\n" " " preview) "\n"))))
                          (insert "\n"))
                        (goto-char (point-min))
                        (view-mode)
                        (pop-to-buffer (current-buffer)))))))

              (defun pb-prompt/delete-context-at-point ()
                "Delete the context at point."
                (interactive)
                (when-let ((name (pb-prompt/context-name-at-point)))
                  (when (yes-or-no-p (format "Delete context '%s'? " (string-trim name)))
                    (pb-prompt/delete-saved-context (string-trim name))
                    (pb-prompt/browse-saved-contexts))))

              (defun pb-prompt/load-context-at-point ()
                "Load the context at point, replacing current context."
                (interactive)
                (when-let ((name (pb-prompt/context-name-at-point)))
                  (pb-prompt/load-context (string-trim name))
                  (quit-window)))

              (defun pb-prompt/copy-context-at-point ()
                (interactive)
                (when-let ((name (pb-prompt/context-name-at-point)))
                  (add-to-list 'pb-prompt/context-ring
                               (pb-prompt/with-id
                                (km :type "context"
                                    :name name)))))

              (defun pb-prompt/append-context-at-point ()
                "Append the context at point to current context."
                (interactive)
                (when-let ((name (pb-prompt/context-name-at-point)))
                  (pb-prompt/append-context (string-trim name))
                  (quit-window))))

       (progn :persist

              (defcustom pb-prompt/contexts-file
                (expand-file-name "~/.doom.d/pb/llms/pb-prompt-contexts.el")
                "File where saved contexts are stored between Emacs sessions."
                :type 'file
                :group 'pb-prompt)

              (defun pb-prompt/save-current-context-to-file (filename)
                "Save the current context to FILENAME.
                 This function serializes the current context into a file that can be loaded later.
                 When called interactively, prompts for a filename to save to."
                (interactive "FSave current context to file: ")
                (with-temp-file filename
                  (let ((print-length nil)
                        (print-level nil))
                    (insert ";; pb-prompt current context - automatically generated\n\n")
                    (insert "(setq pb-prompt/context '(\n")
                    (dolist (item pb-prompt/context)
                      (insert (format "  %S\n" item)))
                    (insert "))\n"))
                  (message "Saved current context to %s" filename)))

              (defun pb-prompt/load-context-from-file (filename)
                "Load a context from FILENAME into the current context.
                 This function reads a serialized context from the specified file and loads it,
                 either replacing or merging with the current context depending on user choice.
                 When called interactively, prompts for a filename to load from."
                (interactive "FLoad context from file: ")
                (if (not (file-exists-p filename))
                    (user-error "File %s does not exist" filename)
                  (when (file-readable-p filename)
                    (let ((loaded-context nil)
                          (buffer (find-file-noselect filename t)))
                      (unwind-protect
                          (with-current-buffer buffer
                            (goto-char (point-min))
                            (setq loaded-context (eval (read buffer)))
                            (unless (and (listp loaded-context)
                                         (cl-every #'km? loaded-context))
                              (user-error "File does not contain a valid context structure")))
                        (kill-buffer buffer))

                      ;; Ask user what to do with the loaded context
                      (if (and pb-prompt/context
                               (not (seq-empty-p pb-prompt/context)))
                          (let ((choice (read-char-choice
                                         "What to do with loaded context? [r]eplace, [a]ppend, [m]erge, [c]ancel: "
                                         '(?r ?a ?m ?c))))
                            (cond
                             ((eq choice ?r)
                              (setq pb-prompt/context loaded-context)
                              (message "Replaced current context with %d loaded items" (length loaded-context)))
                             ((eq choice ?a)
                              (setq pb-prompt/context (append pb-prompt/context loaded-context))
                              (message "Appended %d items to current context" (length loaded-context)))
                             ((eq choice ?m)
                              ;; Merge by adding items that don't have duplicate IDs
                              (let ((merged-count 0))
                                (dolist (item loaded-context)
                                  (when (pb-prompt/add-context-item pb-prompt/context item)
                                    (setq merged-count (1+ merged-count))))
                                (message "Merged %d unique items into current context" merged-count)))
                             (t
                              (message "Context loading cancelled"))))
                        ;; No existing context, just load
                        (setq pb-prompt/context loaded-context)
                        (message "Loaded context with %d items" (length loaded-context)))))))

              (defun pb-prompt/save-contexts-to-file ()
                "Save all contexts to `pb-prompt/contexts-file'."
                (interactive)
                (with-temp-file pb-prompt/contexts-file
                  (let ((print-length nil)
                        (print-level nil))
                    (insert ";; pb-prompt saved contexts - automatically generated\n\n")
                    (insert "(setq pb-prompt/saved-contexts (make-hash-table :test 'equal))\n\n")
                    (maphash (lambda (name context)
                               (insert (format "(puthash %S '(" (substring-no-properties name)))
                               (dolist (item context)
                                 (insert (format "\n  %S" item)))
                               (insert ")\n pb-prompt/saved-contexts)\n\n"))
                             pb-prompt/saved-contexts))
                  (message "Saved contexts to %s" pb-prompt/contexts-file)))

              (defun pb-prompt/load-contexts-from-file ()
                "Load contexts from `pb-prompt/contexts-file'."
                (interactive)
                (when (file-exists-p pb-prompt/contexts-file)
                  (load-file pb-prompt/contexts-file)
                  (message "Loaded %d saved contexts" (hash-table-count pb-prompt/saved-contexts))))

              ;; Automatically load saved contexts when package is loaded
              (with-eval-after-load 'pb-prompt
                (pb-prompt/load-contexts-from-file))

              ;; Add hook to save contexts when Emacs exits
              (add-hook 'kill-emacs-hook #'pb-prompt/save-contexts-to-file)))

(progn :git-utils

       (require 'pb-git)
       (require 'magit)

       (defun pb-prompt/generate-commit-message ()
         "Generate a commit message using GPT from the current magit diff.
          Uses the magit diff buffer to create a prompt with guidelines for commit message
          format, then inserts the response into the commit message buffer."
         (interactive)
         (let ((prompt (pb-prompt/mk
                        (km :instructions
                            (km :base "You are writing clear and concise git commit messages."
                                :task "Generate a git commit message for the changes shown in the diff."
                                :guidelines ["Include a brief summary in the first line (preferably under 50 characters)"
                                             "Start with a capitalized verb in imperative mood (e.g., 'Add', 'Fix', 'Update')"
                                             "You can add a more detailed description after a blank line if needed"]
                                :diff (pb-git/get-diff-string))))))

           ;; Send the request to generate a commit message
           (gptel-request
               prompt
             :callback
             (lambda (response _info)
               ;; Find the magit commit message buffer
               (when-let ((commit-buffer (pb-git/magit-commit-buffer)))
                 (with-current-buffer commit-buffer
                   ;; Insert the generated message at the beginning of the buffer
                   (goto-char (point-min))
                   (when (save-excursion (re-search-forward "^#" nil t))
                     (delete-region (point-min) (1- (match-beginning 0))))
                   (insert response)
                   (insert "\n")
                   ;; Notify the user
                   (message "Generated commit message inserted in magit commit buffer")))))))

       (defun pb-prompt/commit ()
         "Create a new commit with an AI-generated commit message.
          Starts the commit process and uses a timer to generate the message after
          the commit buffer is created."
         (interactive)
         (magit-commit-create)
         (run-with-timer 0.5 nil #'pb-prompt/generate-commit-message))

       (defun pb-prompt/commit-amend ()
         "Amend the current commit with an AI-generated commit message.
          Opens the amend commit buffer and uses a timer to generate a new
          commit message based on the updated diff."
         (interactive)
         (magit-commit-amend)
         (run-with-timer 0.5 nil #'pb-prompt/generate-commit-message))

       (defun pb-prompt/diff-branch ()
         "Compare current branch with another and discuss changes with GPT.
          Prompts for a branch to compare against, creates a diff, and opens a chat
          buffer with the diff content to analyze changes using GPT."
         (interactive)
         (with-temp-buffer
           (let* ((current-branch (magit-get-current-branch))
                  (branches (magit-list-branch-names))
                  (selected-branch (completing-read
                                    (format "Compare %s with branch: " current-branch)
                                    (seq-remove (lambda (branch) (string= branch current-branch))
                                                branches)
                                    nil t))
                  (chat-buffer-name (format "CHAT_DIFF_%s_%s" current-branch selected-branch))
                  (diff-buffer (magit-diff-range selected-branch))
                  (diff-output (with-current-buffer diff-buffer
                                 (buffer-string))))
             ;; Now create a chat buffer with the diff content
             (with-current-buffer (get-buffer-create chat-buffer-name)
               (org-mode)
               (erase-buffer)
               (gptel-mode)
               (setq-local gptel--system-message
                           (pb-prompt/mk
                            (km :context
                                (km :diff-content
                                    (km :current-branch current-branch
                                        :compared-branch selected-branch
                                        :diff diff-output))
                                :instructions
                                (km :base "You are a helpful code assistant analyzing git diffs."
                                    :response-format ["Your response will be inserted in an org buffer, it should be valid org content"
                                                      "All org headings are level 3, 4, 5 ..."
                                                      "Org code blocks should use the syntax: #+begin_src <lang>\n<code block content>\n#+end_src"]
                                    :task "Analyze this git diff and provide a summary of the changes.")))
                           gptel-use-tools nil
                           gptel-max-tokens 32768)

               (evil-normal-state)
               (symex-mode -1)

               ;; Add key bindings similar to other chat buffers
               (evil-define-key nil 'local (kbd "s-q <return>")
                 (lambda () (interactive)
                   (call-interactively #'gptel-send)
                   (evil-insert-newline-below)
                   (goto-char (point-max))))

               ;; Insert the header content
               (insert (format "* Diff: %s..%s\n\n" current-branch selected-branch))
               (insert "** Analyze this diff between branches\n\n")

               ;; Position for GPT response
               (goto-char (point-max))

               ;; Switch to the buffer and start in insert mode
               (switch-to-buffer (current-buffer))
               (evil-insert-state)
               (gptel-send))))))

(provide 'pb-prompt)

;;; examples

[(pb-prompt/update-focused-context
  (lambda (ctx)
    (seq-filter (lambda (item)
                  (km/get item :type))
                ctx)))

 (pb-prompt/context-prompt)
 (setq pb-prompt/context ())
 (car pb-prompt/context)

 (let ((m pb-prompt/context))
   (and (listp m) (cl-every #'km? m)))

 (km/pp pb-prompt/context)

 (km/pp (list (km :a 1)
              (list 3 4 5)))

 (pb/comment
  (file-name-directory "~/pouet/qux/ids")
  (file-directory-p "~/.doom.d/pb")
  (file-name-directory "~/.doom.d/pb")
  (file-name-nondirectory "~/.doom.d/pb")
  (pb-prompt/describe-path "~/.doom.d/pb")
  (pb-gptel/directory-to-km "~/.doom.d/pb")
  (file-exists-p "/Users/pierrebaille/.doom.d/pb/archived/reaper.el"))

 (pb/comment
  (pb-tree/get-path-values pb-prompt/tree [:code :lisp :context]))

 ]



(pb/comment
 :interactive-request

 (defun pb-gptel/simple-select-paths (prompt m)
   (interactive)
   (let* ((path-strs (mapcar (lambda (p)
                               (intern (mapconcat #'pb/keyword-name (car p) ".")))
                             (km/all-paths m))))
     (mapcar (lambda (k)
               (mapcar #'intern
                       (mapcar (lambda (s) (concat ":" s))
                               (split-string k "\\."))))
             (completing-read-multiple prompt path-strs))))

 (defun pb-gptel/select-paths (prompt m)
   "Select paths from a map M using PROMPT with aligned annotations.
    Provides completion with vertically aligned hints showing each path's content."
   (interactive)
   (let* ((flatten-tree
           (seq-reduce (pb/fn [m (cons path content)]
                              (km/put m
                                      (pb/keyword (mapconcat #'pb/keyword-name path "."))
                                      (truncate-string-to-width
                                       (pb/if
                                        (stringp content) content
                                        (functionp content) (or (documentation content) "#<function>")
                                        (listp content) "#<plist>"
                                        (format "%s" content))
                                       100 nil nil "...")))
                       (km/all-paths m)
                       ()))
          (completion-extra-properties
           (km :affixation-function
               (lambda (candidates)
                 (let ((max-len (apply #'max (mapcar #'length candidates))))
                   (mapcar (lambda (cand)
                             ;; (print cand)
                             (let ((content (km/get flatten-tree (intern cand)))
                                   (segments (split-string cand "\\." t)))
                               (list (concat (propertize (mapconcat #'identity
                                                                    (sq/butlast segments)
                                                                    ".")
                                                         'face 'font-lock-comment-face)
                                             (if (cdr segments) ".")
                                             (sq/last segments))
                                     ""
                                     (when content
                                       (concat (make-string (- max-len (length cand) -2) ?\s)
                                               (propertize content 'face 'font-lock-comment-face))))))
                           candidates)))))
          (crm-separator "[ 	]* [ 	]*"))
     (mapcar (lambda (k)
               (mapcar #'pb/keyword
                       (split-string (substring k 1) "\\.")))
             (completing-read-multiple prompt (km/keys flatten-tree)))))

 (defun pb-gptel/sub-request-tree ()
   (interactive)
   (let* ((selected-paths (pb-gptel/select-paths "Select request-tree paths: " pb-gptel/request-tree)))
     (km/select-paths* pb-gptel/request-tree selected-paths)))

 (defun pb-gptel/interactive-request ()
   (interactive)
   (let* ((req (pb-gptel/sub-request-tree))
          (action (read-char-choice
                   "i = insert, r = replace, b = buffer: "
                   '(?i ?r ?b))))
     (pb-gptel/request
      req
      (km :callback
          (lambda (res info)
            (cond
             ((eq action ?i)
              ;; Insert at point
              (insert res))

             ((eq action ?r)
              ;; Replace region if active, current line if no region, or current symex in symex-mode
              (cond
               ;; If we're in symex-mode, replace the current symex
               ((and (boundp 'symex-mode) symex-mode)
                (symex--undo-collapse-begin 'pb-gptel/replace-symex)
                (symex-change 1)
                (insert res)
                (symex-mode-interface)
                (symex-tidy)
                (symex--undo-collapse-end 'pb-gptel/replace-symex))

               ;; If a region is active, replace it
               ((use-region-p)
                (delete-region (region-beginning) (region-end))
                (insert res))

               ;; Otherwise replace the current line
               (t
                (beginning-of-line)
                (let ((line-end (line-end-position)))
                  (delete-region (point) line-end)
                  (insert res)))))

             ((eq action ?b)
              ;; Create new buffer with response
              (let ((buffer-name (generate-new-buffer-name
                                  (format "GPTEL Result: %s"
                                          (file-name-base (or (buffer-file-name) "response"))))))
                (with-current-buffer (get-buffer-create buffer-name)
                  (insert res)
                  (funcall major-mode)
                  (goto-char (point-min))
                  ))))))))))
