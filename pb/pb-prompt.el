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

(defun pb-prompt/add-path ()
  (interactive)
  (let ((path (read-file-name "Select directory or file: " nil nil t)))
    (when path
      (setq pb-prompt/context
            (cons (km :type (cond
                             ((file-directory-p path) "dir")
                             ((file-regular-p path) "file")
                             (t "unknown"))
                      :path path)
                  pb-prompt/context)))))

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
           (pb-symex_current-as-string))

          ;; Default fallback message
          (t
           (user-error "No selection or symex available")))))
    (when selection
      (setq pb-prompt/context
            (cons (km :type "selection"
                      :path (buffer-file-name)
                      :at (point)
                      :content selection)
                  pb-prompt/context)))))

(defun pb-prompt/add-buffer ()
  "Add the current buffer to the prompt context.
   This collects the buffer name, file path, major mode, and content
   for inclusion in the prompt context."
  (interactive)
  (setq pb-prompt/context
        (cons (km :type "buffer"
                  :path (buffer-file-name)
                  :buffer-name (buffer-name)
                  :major-mode (symbol-name major-mode))
              pb-prompt/context)))

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
             (t ctx-item))))
        pb-prompt/context))))

(defun pb-prompt/remove-context-items ()
  "Let the user interactively remove some items from pb-prompt/context.
   The items are grouped by type in the completion UI via category property."
  (interactive)
  (when pb-prompt/context
    (let* ((items-by-type (seq-group-by
                           (lambda (item) (km_get item :type))
                           pb-prompt/context))
           ;; Create candidates with proper properties for grouping
           (candidates (mapcan
                        (lambda (type-group)
                          (let ((type (car type-group))
                                (items (cdr type-group)))
                            (mapcar
                             (lambda (item)
                               (let* ((path (km_get item :path))
                                      (desc (cond
                                             ((string= type "buffer")
                                              (km_get item :buffer-name))
                                             ((string= type "file")
                                              path)
                                             ((string= type "dir")
                                              path)
                                             ((string= type "selection")
                                              path)
                                             (t (format "Item of type: %s" type)))))
                                 ;; Store item as property for retrieval later
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
           ;; Define annotation function
           (annotate-function (lambda (cand)
                                (let ((type (get-text-property 0 'consult--group cand)))
                                  (concat " " (propertize type 'face 'marginalia-type)))))
           ;; Get selections using consult--read
           (to-remove (mapcar (lambda (cand)
                                (get-text-property 0 'context-item cand))
                              (consult--read candidates
                                             :prompt "Select items to remove: "
                                             :category 'context-item
                                             :group group-function
                                        ;:annotate annotate-function
                                             :require-match t
                                             :sort nil))))

      ;; Update the context by removing selected items
      (setq pb-prompt/context
            (seq-difference pb-prompt/context to-remove #'equal))
      (message "Removed %d item(s) from context" (length to-remove)))))



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
