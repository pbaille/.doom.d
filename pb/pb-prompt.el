;;; pb-prompt.el --- crafting prompts for LLMs -*- lexical-binding: t; -*-

;;; Commentary:
;; This package provides tools for creating and managing prompts for large
;; language models (LLMs).  It allows users to create structured prompts
;; with a tree-based organization system, making it easier to craft
;; context-specific interactions with LLMs.
;;
;; The main features include:
;; - Prompt formatting with the `pb-prompt_mk` function
;; - File and directory description functionality
;; - A tree-based prompt organization system

;;; Code:

(require 'km)
(require 'pb)
(require 'pb-tree)
(require 'pb-symex)

(defvar pb-prompt/context ())

(defvar pb-prompt_tree

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
                                         (km :pb (pb_slurp "~/.doom.d/pb/pb.el")
                                             :km (pb_slurp "~/.doom.d/pb/km.el")))))

                       :context
                       (lambda ()
                         "Include code context, whole file, current-expression and more..."
                         (km :buffer-name (buffer-file-name)
                             :major-mode (symbol-name major-mode)
                             :file-content (buffer-substring-no-properties (point-min) (point-max))
                             :current-expression (pb-symex_current-as-string))))

                 :fill
                 "Complete the holes (denoted by __) in the given expression, do not change anything else!")

           :task (lambda ()
                   "Enter main instructions."
                   (read-string "main task: "))))

(defun pb-prompt/indent-content (content &optional indent-size)
  "Indent each line of CONTENT with spaces if it contains newlines.
   If CONTENT is a single line, return it unchanged.
   Optional argument INDENT-SIZE specifies the number of spaces to use (defaults to 2)."
  (if (string-match-p "\n" content)
      (let ((spaces (make-string (or indent-size 2) ?\s)))
        (replace-regexp-in-string
         "^\\(.\\)" (concat spaces "\\1") content))
    content))



(defun pb-prompt_mk (x)
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
  (cond ((stringp x) x)
        ((functionp x) (pb-prompt_mk (funcall x)))
        ((km? x)
         (mapconcat (lambda (entry)
                      (let* ((key-str (substring (symbol-name (car entry)) 1))
                             (content (pb-prompt_mk (cdr entry))))
                        ;; Format each entry as XML-like tags with indented content
                        (concat "<" key-str ">\n"
                                (pb-prompt/indent-content content 2)
                                "\n</" key-str ">")))
                    (km_entries x)
                    "\n\n")) ; Execute functions to get their content
        ((vectorp x) (mapconcat #'identity x "\n"))
        ((listp x) (mapconcat (lambda (item)
                                (concat "<context-item>\n"
                                        (pb-prompt/indent-content
                                         (pb-prompt_mk item)
                                         2)
                                        "\n</context-item>"))
                              x
                              "\n"))))

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
                                      (pb_let [(as x (km_keys name))
                                               (pb-prompt/describe-path p)]
                                        (if name
                                            (km_put ret (pb_keyword name) x)
                                          ret)))
                                    (directory-files path t "^[^.].*")
                                    ()))
        (km :name (file-name-nondirectory path)
            :type "file"
            :path path
            :content (pb_slurp path)))))

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
         (km_put context-item
                 :id
                 (substring
                  (md5 (format "%s" (time-convert nil 'list)))
                  0 8)))

       (defun pb-prompt/add-item! (item)
         "Add ITEM to context with a unique identifier.

          This function attaches a unique :id key to ITEM before adding it to
          `pb-prompt/context'. The ID is generated using MD5 hash of the current
          timestamp, ensuring uniqueness across context items. Items are added
          to the front of the context list, making more recent additions appear
          first when processing context items later.

          Example:
          (pb-prompt/add-item! (km :type \"selection\" :content \"my code\"))
          ;; Adds selection with a unique ID to the context"
         (setq pb-prompt/context
               (cons (pb-prompt/with-id item)
                     pb-prompt/context)))

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

       (defun pb-prompt/add-selection ()
         "Add the current selection to the prompt context.
          Adds either the active region or current symex if in symex-mode."
         (interactive)
         (let ((selection
                (cond
                 ;; If region is active, use region
                 ((use-region-p)
                  (buffer-substring-no-properties (region-beginning) (region-end)))

                 ;; If we're in symex-mode, use current symex
                 ((and (boundp 'symex-mode) symex-mode)
                  (pb-symex_current-as-string t))

                 ;; Default fallback message
                 (t
                  (user-error "No selection or symex available")))))
           (when selection
             (pb-prompt/add-item!
              (km :type "selection"
                  :path (buffer-file-name)
                  :major-mode major-mode
                  :symex symex-mode
                  :at (point)
                  :content selection)))))

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

       pb-prompt/context
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
                    (selected-func (symbol-function
                                    (intern (completing-read
                                             "Select function: "
                                             obarray-functions
                                             #'fboundp t)))))
               (pb-prompt/add-item!
                (km :type "function"
                    :name selected-func
                    :function (symbol-function selected-func)
                    :documentation (or (documentation func) "No documentation available")))))

            ;; Custom lambda input
            ((eq choice ?c)
             (let* ((temp-buffer-name "*Lambda Function Editor*")
                    (buffer (get-buffer-create temp-buffer-name))
                    (func-name (read-string "Function name: ")))
               (with-current-buffer buffer
                 (erase-buffer)
                 (emacs-lisp-mode)
                 (insert ";; Create a lambda function to generate context\n")
                 (insert ";; Press C-c C-c when done to continue\n;; Press C-c C-k to cancel and exit\n\n")
                 (insert "(lambda ()\n (interactive)\n ())")
                 (goto-char (point-max))
                 (backward-char 2)
                 (symex-mode-interface)
                 (let ((map (make-sparse-keymap)))
                   (define-key map (kbd "C-c C-c")
                               (lambda ()
                                 (interactive)
                                 (let* ((func-expr (buffer-substring-no-properties
                                                    (save-excursion
                                                      (goto-char (point-min))
                                                      (search-forward "(lambda"))
                                                    (point-max)))
                                        (func-expr (concat "(lambda" func-expr))
                                        (func (condition-case err
                                                  (eval (read func-expr))
                                                (error
                                                 (message "Error in lambda: %s" (error-message-string err))
                                                 nil))))
                                   (kill-buffer buffer)
                                   (when (functionp func)
                                     (message "Function defined successfully")
                                     (pb-prompt/add-item!
                                      (km :type "function"
                                          :name func-name
                                          :function func
                                          :documentation (or (documentation func) "No documentation available")))))))
                   (define-key map
                               (kbd "C-c C-k")
                               (lambda () (interactive) (kill-buffer buffer)))
                   (use-local-map (make-composed-keymap map (current-local-map))))
                 (message "Edit lambda function in buffer. Press C-c C-c when done"))
               (switch-to-buffer buffer)))))))

(defun pb-prompt/context-prompt ()
  "Generate a prompt from the current context.
   This function formats the collected context elements into a structured
   prompt suitable for an LLM, using the pb-prompt_mk function."
  (interactive)
  (pb-prompt_mk
   (km :context
       (mapcar
        (lambda (ctx-item)
          (let ((type (km_get ctx-item :type)))
            (cond
             ((string= type "buffer")
              (km_put ctx-item
                      :content (with-current-buffer (km_get ctx-item :buffer-name)
                                 (buffer-substring-no-properties (point-min) (point-max)))))
             ((member type (list "file" "dir"))
              (pb-prompt/describe-path (km_get ctx-item :path)))

             ((string= type "function")
              (call-interactively (km_get ctx-item :function)))

             (t ctx-item))))
        pb-prompt/context))))

;; (pb-prompt/context-prompt)

(progn :consult-context

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
         (let ((type (km_get item :type))
               (path (km_get item :path)))
           (cond
            ((string= type "buffer")
             (km_get item :buffer-name))
            ((member type (list "file" "dir"))
             (pb-prompt/-format-relative-path path))
            ((string= type "selection")
             (let* ((content (km_get item :content))
                    (first-line (car (split-string content "\n")))
                    (truncated-line (truncate-string-to-width first-line 80 nil nil "...")))
               truncated-line))
            ((string= type "function")
             (let* ((content (km_get item :documentation))
                    (first-line (car (split-string content "\n")))
                    (truncated-line (truncate-string-to-width first-line 80 nil nil "...")))
               (concat (km_get item :name) " :: " truncated-line)))
            (t (format "Item of type: %s" type)))))

       (defun pb-prompt/select-context-item ()
         (interactive)
         (when pb-prompt/context
           (let* ((items-by-type (seq-group-by
                                  (lambda (item) (km_get item :type))
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

       (defun pb-prompt/remove-context-item (&optional item)
         "Let the user interactively remove some items from pb-prompt/context.
          The items are grouped by type in the completion UI via category property."
         (interactive)
         (if-let ((to-remove (or item (pb-prompt/select-context-item))))

             (progn (message "Removing context item: %s (ID: %s)"
                             (or (km_get to-remove :type) "unknown")
                             (km_get to-remove :id))
                    (setq pb-prompt/context
                          (seq-remove (lambda (item)
                                        (equal (km_get item :id) (km_get to-remove :id)))
                                      pb-prompt/context)))

           (message "Warning: No item selected for removal (Key: %s)"
                    picked)))

       (defun pb-prompt/browse-context-item (&optional item)
         "Open or display the content of the selected context item."
         (interactive)
         (let* ((item (or item (pb-prompt/select-context-item)))
                (type (km_get item :type)))
           (cond
            ((string= type "buffer")
             (switch-to-buffer (km_get item :buffer-name)))
            ((string= type "file")
             (find-file (km_get item :path)))
            ((string= type "dir")
             (dired (km_get item :path)))
            ((string= type "selection")
             (with-current-buffer (get-buffer-create "*Context Item Selection*")
               (erase-buffer)
               (insert (km_get item :content))
               (goto-char (point-min))
               (when-let ((path (km_get item :path))
                          (mode (km_get item :major-mode)))
                 (funcall mode)
                 (when (km_get item :symex)
                   (symex-mode-interface)
                   (symex-tidy)))
               (switch-to-buffer (current-buffer))))
            (t (message "Don't know how to browse item of type: %s" type)))))

       (defun pb-prompt/describe-context-item (&optional item)
         "Show detailed information about the context item."
         (with-current-buffer (get-buffer-create "*Context Item Details*")
           (erase-buffer)
           (let* ((item (or item (pb-prompt/select-context-item)))
                  (id (km_get item :id))
                  (type (km_get item :type))
                  (path (km_get item :path)))
             (insert (format "ID: %s\n" id)
                     (format "Type: %s\n" type))
             (when path
               (insert (format "Path: %s\n" path)))

             (dolist (key (mapcar #'car (km_entries item)))
               (unless (member key '(:id :type :path :content))
                 (insert (format "%s: %s\n"
                                 (substring (symbol-name key) 1)
                                 (km_get item key)))))

             (when-let ((content (km_get item :content)))
               (insert "\nContent:\n---------\n")
               (insert content)))
           (goto-char (point-min))
           (pop-to-buffer (current-buffer))))

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

(progn :context-persist

       (defvar pb-prompt/saved-contexts
         (make-hash-table :test 'equal))

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

       (progn :saved-contexts

              (defun pb-prompt/list-saved-contexts ()
                "Show a list of all saved contexts with item counts."
                (interactive)
                (with-current-buffer (get-buffer-create "*Saved Contexts*")
                  (erase-buffer)
                  (let ((contexts (hash-table-keys pb-prompt/saved-contexts))
                        (inhibit-read-only t))
                    (if (null contexts)
                        (insert "No saved contexts found.\n")
                      (insert "Saved Contexts:\n\n")
                      (dolist (name (sort contexts #'string<))
                        (let* ((context (gethash name pb-prompt/saved-contexts))
                               (count (length context))
                               (types (mapcar (lambda (item) (km_get item :type)) context))
                               (type-counts (seq-reduce
                                             (lambda (acc type)
                                               (let ((count (or (alist-get type acc) 0)))
                                                 (setf (alist-get type acc) (1+ count))
                                                 acc))
                                             types
                                             nil)))
                          (insert (format "• %s (%d items)\n" name count))
                          (insert "  Types: ")
                          (let ((type-strs (mapcar (lambda (type)
                                                     (format "%s (%d)"
                                                             (car type)
                                                             (cdr type)))
                                                   type-counts)))
                            (insert (mapconcat #'identity type-strs ", "))
                            (insert "\n\n")))))
                    (goto-char (point-min))
                    (pb-prompt/saved-contexts-mode)
                    (setq-local header-line-format
                                "RET: open, d: delete, l: load, a: append, q: quit"))
                  (pop-to-buffer (current-buffer))))

              ;; Define a specialized mode for saved contexts buffer
              (define-derived-mode pb-prompt/saved-contexts-mode special-mode "PB-Prompt Contexts"
                "Major mode for listing saved prompt contexts."
                (setq buffer-read-only t))

              ;; Create a keymap for the mode that works with evil
              (defvar pb-prompt/saved-contexts-mode-map
                (let ((map (make-sparse-keymap)))
                  (define-key map (kbd "RET") #'pb-prompt/open-context-at-point)
                  (define-key map (kbd "d") #'pb-prompt/delete-context-at-point)
                  (define-key map (kbd "l") #'pb-prompt/load-context-at-point)
                  (define-key map (kbd "a") #'pb-prompt/append-context-at-point)
                  (define-key map (kbd "q") #'quit-window)
                  map)
                "Keymap for `pb-prompt/saved-contexts-mode'.")

              ;; Ensure evil respects our keymap in normal state
              (with-eval-after-load 'evil
                (evil-set-initial-state 'pb-prompt/saved-contexts-mode 'normal)
                (evil-define-key 'normal pb-prompt/saved-contexts-mode-map
                  (kbd "RET") #'pb-prompt/open-context-at-point
                  (kbd "d") #'pb-prompt/delete-context-at-point
                  (kbd "l") #'pb-prompt/load-context-at-point
                  (kbd "a") #'pb-prompt/append-context-at-point
                  (kbd "q") #'pb-prompt/exit-saved-contexts))

              (defun pb-prompt/context-name-at-point ()
                "Get context name at point in the saved contexts buffer."
                (save-excursion
                  (beginning-of-line)
                  (when (looking-at "• \\([^(]+\\)")
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
                          (insert (format "Type: %s\n" (km_get item :type)))
                          (when-let ((id (km_get item :id)))
                            (insert (format "ID: %s\n" id)))
                          (when-let ((path (km_get item :path)))
                            (insert (format "Path: %s\n" path)))
                          (let ((content (km_get item :content)))
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
                    (pb-prompt/list-saved-contexts))))

              (defun pb-prompt/load-context-at-point ()
                "Load the context at point, replacing current context."
                (interactive)
                (when-let ((name (pb-prompt/context-name-at-point)))
                  (pb-prompt/load-context (string-trim name))
                  (quit-window)))

              (defun pb-prompt/append-context-at-point ()
                "Append the context at point to current context."
                (interactive)
                (when-let ((name (pb-prompt/context-name-at-point)))
                  (pb-prompt/append-context (string-trim name))
                  (quit-window)))

              (defun pb-prompt/exit-saved-contexts ()
                "Exit the saved contexts buffer, kill the buffer, and quit the window."
                (interactive)
                (let ((buffer (current-buffer)))
                  (quit-window t)
                  (kill-buffer buffer))))

       (progn :persist-contexts

              (quote
               (find-file
                (expand-file-name "pb-prompt-contexts.el" user-emacs-directory)))

              (defcustom pb-prompt/contexts-file
                (expand-file-name "pb-prompt-contexts.el" user-emacs-directory)
                "File where saved contexts are stored between Emacs sessions."
                :type 'file
                :group 'pb-prompt)

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
              (add-hook 'kill-emacs-hook #'pb-prompt/save-contexts-to-file))
)

(provide 'pb-prompt)


[
 (pb-prompt/context-prompt)
 (setq pb-prompt/context ())
 (car pb-prompt/context)

 (let ((m pb-prompt/context))
   (and (listp m) (cl-every #'km? m)))

 (km_pp pb-prompt/context)

 (km_pp (list (km :a 1)
              (list 3 4 5)))

 (pb_comment
  (file-name-directory "~/pouet/qux/ids")
  (file-directory-p "~/.doom.d/pb")
  (file-name-directory "~/.doom.d/pb")
  (file-name-nondirectory "~/.doom.d/pb")
  (pb-prompt_describe-path "~/.doom.d/pb")
  (pb-gptel/directory-to-km "~/.doom.d/pb")
  (file-exists-p "/Users/pierrebaille/.doom.d/pb/archived/reaper.el"))

 (pb_comment
  (pb-tree_get-path-values pb-prompt_tree [:code :lisp :context])

  (pb-tree_select pb-prompt_tree [:code :lisp :context]))]










(pb_comment
 :interactive-request

 (defun pb-gptel/simple-select-paths (prompt m)
   (interactive)
   (let* ((path-strs (mapcar (lambda (p)
                               (intern (mapconcat #'pb_keyword-name (car p) ".")))
                             (km_all-paths m))))
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
           (seq-reduce (pb_fn [m (cons path content)]
                              (km_put m
                                      (pb_keyword (mapconcat #'pb_keyword-name path "."))
                                      (truncate-string-to-width
                                       (pb_if
                                        (stringp content) content
                                        (functionp content) (or (documentation content) "#<function>")
                                        (listp content) "#<plist>"
                                        (format "%s" content))
                                       100 nil nil "...")))
                       (km_all-paths m)
                       ()))
          (completion-extra-properties
           (km :affixation-function
               (lambda (candidates)
                 (let ((max-len (apply #'max (mapcar #'length candidates))))
                   (mapcar (lambda (cand)
                             ;; (print cand)
                             (let ((content (km_get flatten-tree (intern cand)))
                                   (segments (split-string cand "\\." t)))
                               (list (concat (propertize (mapconcat #'identity
                                                                    (sq_butlast segments)
                                                                    ".")
                                                         'face 'font-lock-comment-face)
                                             (if (cdr segments) ".")
                                             (sq_last segments))
                                     ""
                                     (when content
                                       (concat (make-string (- max-len (length cand) -2) ?\s)
                                               (propertize content 'face 'font-lock-comment-face))))))
                           candidates)))))
          (crm-separator "[ 	]* [ 	]*"))
     (mapcar (lambda (k)
               (mapcar #'pb_keyword
                       (split-string (substring k 1) "\\.")))
             (completing-read-multiple prompt (km_keys flatten-tree)))))

 (defun pb-gptel/sub-request-tree ()
   (interactive)
   (let* ((selected-paths (pb-gptel/select-paths "Select request-tree paths: " pb-gptel/request-tree)))
     (km_select-paths* pb-gptel/request-tree selected-paths)))

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
