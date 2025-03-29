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
                                (if (string-match-p "\n" content)
                                    (replace-regexp-in-string
                                     "^\\(.\\)" "  \\1"
                                     content) ; Indent multiline content for better readability
                                  content)
                                "\n</" key-str ">")))
                    (km_entries x)
                    "\n\n")) ; Execute functions to get their content
        ((vectorp x) (mapconcat #'identity x "\n"))
        ((listp x) (prin1-to-string x))))

(defun pb-prompt_describe-path (path)
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
  (when (and path (file-exists-p path))
    (let ((filename (file-name-nondirectory path)))
      (if (file-directory-p path)
          (list 'dir
                :name filename
                :children (seq-reduce (lambda (ret p)
                                        (pb_let [(as x (km_keys name))
                                                 (pb-gptel/describe-path p)]
                                          (print name)
                                          (km_put ret (pb_keyword name) x)))
                                      (directory-files path t "^[^.].*")
                                      ()))
        (km :name filename
            :type "file"
            :content :pouet ;; (pb_slurp path)
            )))))

(pb_comment
 (pb-gptel/describe-path "~/.doom.d/pb"))

(pb_comment
 (pb-tree_get-path-values pb-prompt_tree [:code :lisp :context])

 (pb-tree_select pb-prompt_tree [:code :lisp :context]))


(provide 'pb-prompt)










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
