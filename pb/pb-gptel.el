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
(require 'km)

(defvar pb-gptel_history-dir
  "~/.doom.d/chats/history/")

(defvar pb-gptel_buffer-name-prefix
  "GPTEL_")

(defun pb-gptel_mk-prompt (pl)
  "Convert a keyword map (plist) into an XML-like formatted prompt string.

Each key-value pair in PL becomes an XML-like tag structure where:
- Keys become tag names (with ':' prefix removed)
- Values are processed based on their type:
  - Strings are used directly
  - Nested keyword maps are recursively processed
  - Functions are executed and their results processed
  - Lists are converted to strings using `prin1-to-string`
  - Vectors are joined with newlines

The content within tags is indented for better readability when it contains
multiple lines."
  ;; Process each key-value pair in the plist and join with newlines
  (mapconcat (lambda (entry)
               (let* ((key-str (substring (symbol-name (car entry)) 1))
                      (val (cdr entry))
                      ;; Recursively convert nested structures based on their type
                      (content (cond
                                ((stringp val) val)
                                ((km? val) (pb-gptel_mk-prompt val)) ; Recursively process nested km maps
                                ((functionp val) (pb-gptel_mk-prompt (funcall val))) ; Execute functions to get their content
                                ((listp val) (prin1-to-string val))
                                ((vectorp val) (mapconcat #'identity val "\n")))))
                 ;; Format each entry as XML-like tags with indented content
                 (concat "<" key-str ">\n"
                         (if (string-match-p "\n" content)
                             (replace-regexp-in-string
                              "^\\(.\\)" "  \\1"
                              content) ; Indent multiline content for better readability
                           content)
                         "\n</" key-str ">")))
             (km_entries pl)
             "\n\n"))

(defun pb-gptel_request (content options)
  "Send GPT request with formatted CONTENT and OPTIONS.
CONTENT is a plist or keyword map that will be formatted into an XML-like
prompt string using `pb-gptel_mk-prompt`.
OPTIONS is a plist of options to be passed to `gptel-request`.

When called interactively, this prompts for the necessary inputs."
  (interactive)
  (apply #'gptel-request
         (pb-gptel_mk-prompt content)
         options))

(defvar pb-gptel_tree

  (km :lisp
      "You are a useful code assistant, you really like lisp-like languages and you know how to balance parentheses correctly."

      :clj
      "You are a Clojure expert who understands functional programming concepts and persistent data structures."

      :cljs
      "You are a ClojureScript expert who understands both Clojure concepts and JavaScript interoperability. You're familiar with the React paradigm and modern frontend development patterns."

      :code
      ["Your response should be valid code, intended to replace the current expression in a source code file."
       "Don't use markdown code block syntax or any non-valid code in your output."]

      :fill
      "Complete the holes (denoted by __) in the given expression, do not change anything else!"

      :code
      (km :symex-context
          (lambda ()
            "Include code context, whole file, current-expression and more..."
            (km :editor "emacs"
                :buffer-name (buffer-file-name)
                :major-mode (symbol-name major-mode)
                :file-content (buffer-substring-no-properties (point-min) (point-max))
                :current-expression (pb-symex_current-as-string))))

      :task (lambda ()
              "Enter main instructions."
              (read-string "main task: "))))

(defun pb-gptel_get (&rest path)
  "Retrieve a value from the request tree using the given PATH.
PATH is a sequence of keys to navigate `pb-gptel_request-tree`.
Each element in PATH represents a level in the tree hierarchy.

Returns the value at the specified path or nil if not found."
  (km_get pb-gptel_tree path))

(defun pb-gptel_put (&rest put-args)
  "Update the GPT request tree with the given key-value pairs.
PUT-ARGS are arguments to be passed to `km_put` to update `pb-gptel_tree`.
This function follows the same pattern as `km_put`:
- First arguments are path elements (keys)
- Last argument is the value to set
Returns the updated `pb-gptel_tree`."
  (setq pb-gptel_tree
        (apply #'km_put pb-gptel_tree put-args)))

(progn :interactive-request

       (defun pb-gptel_simple-select-paths (prompt m)
         (interactive)
         (let* ((path-strs (mapcar (lambda (p)
                                     (intern (mapconcat #'pb_keyword-name (car p) ".")))
                                   (km_all-paths m))))
           (mapcar (lambda (k)
                     (mapcar #'intern
                             (mapcar (lambda (s) (concat ":" s))
                                     (split-string k "\\."))))
                   (completing-read-multiple prompt path-strs))))

       (defun pb-gptel_select-paths (prompt m)
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

       (defun pb-gptel_sub-request-tree ()
         (interactive)
         (let* ((selected-paths (pb-gptel_select-paths "Select request-tree paths: " pb-gptel_request-tree)))
           (km_select-paths* pb-gptel_request-tree selected-paths)))

       (defun pb-gptel_interactive-request ()
         (interactive)
         (let* ((req (pb-gptel_sub-request-tree))
                (action (read-char-choice
                         "i = insert, r = replace, b = buffer: "
                         '(?i ?r ?b))))
           (pb-gptel_request
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
                      (symex--undo-collapse-begin 'pb-gptel_replace-symex)
                      (symex-change 1)
                      (insert res)
                      (symex-mode-interface)
                      (symex-tidy)
                      (symex--undo-collapse-end 'pb-gptel_replace-symex))

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
                        (switch-to-buffer-other-window (current-buffer))))))))))))

(defun pb-gptel_replace-current-symex-request-callback (res info)
  "Replace current symbolic expression with GPT model response.
RES is the response text from the language model.
INFO is the information plist provided by gptel containing request metadata.
Performs proper navigation and cleanup after replacement."
  (symex-change 1)
  (insert res)
  (symex-mode-interface)
  (symex-tidy))

(defun pb-gptel_context-files-to-km ()
  ""
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
      (message "Removed %s from context" selected-file))))

(defun pb-gptel_current-symex-request-replace (&optional instruction)
  "Request llm to replace the current symex based on provided INSTRUCTION..
When called interactively, prompts for an instruction to guide the modification.
If INSTRUCTION is provided as an argument, uses that instead of prompting.
Uses the current expression context to send a formatted request to GPT,
and replaces the current expression with GPT's response when received."
  (interactive)
  (unless instruction
    (setq instruction (read-string "Edit current expression: ")))
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
           :task instruction
           :expression (pb-symex_current-as-string)))

   (km :callback
       #'pb-gptel_replace-current-symex-request-callback)))

(defun pb-gptel_fill-holes ()
  "Fill holes (denoted by __) in the current expression.
This function sends the current expression to the language model with
instructions to complete any placeholder holes marked with __ without
modifying the rest of the code. The response will replace the current
expression."
  (interactive)
  (pb-gptel_current-symex-request-replace
   "Complete the holes (denoted by __) in the given expression, do not change anything else!"))

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
