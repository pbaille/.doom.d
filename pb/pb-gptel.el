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
