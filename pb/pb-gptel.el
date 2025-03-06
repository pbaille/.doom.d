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

 (defun pb-gptel_new-session-above ()
   "Create a new GPT session in a split window above the current one.
 Opens a new Org buffer with gptel-mode activated, creates a new heading,
 and positions the cursor for immediate input in evil insert state."
   (interactive)
   (let* ((buffer (get-buffer-create (concat "GPTEL_" (file-name-base (buffer-file-name))))))
     (with-current-buffer buffer
       (erase-buffer)
       (org-mode)
       (gptel-mode)
       (insert "*** ")
       (point-max)
       (evil-insert-state))
     (pb-misc_window-split buffer)
     (windmove-down)))

 (defun pb-gptel_current-buffer-request-inlined ()
   "Send the current buffer to GPT with user-provided instructions.
 Prompts for instructions, then sends the entire buffer as context to GPT.
 The GPT response will be inserted at the current cursor position."
   (interactive)
   (let* ((instruction (read-string "Instruction for GPT: "))
          (syntax-info (format (concat "SYNTAX_INSTRUCTIONS_START\n\n Format your response as valid code that will be inserted in the CURRENT_FILE which has extension %s and is in %s mode."
                                       "\nYour response should not include CURRENT_FILE code or introduce duplicates. Do not use markdown code blocks, just pure valid code for the CURRENT_FILE."
                                       "\n\nSYNTAX_INSTRUCTIONS_END")
                               (file-name-extension (buffer-file-name) t)
                               (symbol-name major-mode)))
          (file-context (concat "CURRENT_FILE_START\n\n"
                                (buffer-string)
                                "\n\nCURRENT_FILE_END\n\n"))
          (instructions (concat "\n\nMAIN_INSTRUCTIONS_START\n\n"
                                instruction
                                "\n\nMAIN_INSTRUCTIONS_END\n\n")))
     (gptel-request (concat file-context
                            "\n\n"
                            syntax-info
                            "\n\n"
                            instructions)
       :position (point))))

'(defun pb-gptel_current-buffer-request-inlined2 ()
   "Send the current buffer to GPT with user-provided instructions.
 Prompts for instructions, then sends the entire buffer as context to GPT.
 The GPT response will be inserted at the current cursor position."
   (interactive)
   (let* ((instruction (read-string "Instruction for GPT: "))
          (insertion-marker "<<<INSERT_HERE>>>")
          (file-content-with-marker (concat (buffer-substring-no-properties (point-min) (point))
                                            insertion-marker
                                            (buffer-substring-no-properties (point) (point-max))))
          (syntax-info (json-encode (list :syntax_instructions
                                           (list :format (format "valid code for a file with extension %s in %s mode"
                                                                 (file-name-extension (buffer-file-name) t)
                                                                 (symbol-name major-mode))
                                                 :note "\nYour response should not include CURRENT_FILE code or introduce duplicates. Do not use markdown code blocks, just pure valid code for the CURRENT_FILE."))))


          (file-context (json-encode (list :current_file file-content-with-marker)))
          (instructions (json-encode (list :main_instructions instruction))))
     (gptel-request (concat file-context "\n" syntax-info "\n" instructions)
       :position (point))))

 (defun pb-gptel_mk-prompt (pl)
   "Convert a plist into xml like prompt str"
   (interactive)
   (mapconcat (lambda (entry)
                (let* ((key-str (substring (symbol-name (car entry)) 1))
                       (val (cdr entry)))
                  (concat "<" key-str ">\n"
                          (cond
                           ((stringp val) val)
                           ((km? val) (pb-gptel_mk-prompt val))
                           ((functionp val) (pb-gptel_mk-prompt (funcall val)))
                           ((listp val) (prin1-to-string val))
                           ((vectorp val) (mapconcat #'identity val "\n")))
                          "\n</" key-str ">")))
              (km_entries pl)
              "\n\n"))

(defun pb-gptel_request (content options)
  ""
  (interactive)
  (apply #'gptel-request
         (pb-gptel_mk-prompt content)
         options))

(defvar pb-gptel_request-tree
  (km :lisp "You are a useful code assistant, you really like lisp-like languages and you know how to balance parentheses correctly."
      :clj "You are a Clojure expert who understands functional programming concepts and persistent data structures."
      :cljs "You are a ClojureScript expert who understands both Clojure concepts and JavaScript interoperability. You're familiar with the React paradigm and modern frontend development patterns."
      :refactor "You are a code refactoring expert. Improve the given code while preserving its functionality. Focus on clarity, efficiency, and maintainability."
      :fix "Change the parts surrounded by angle brackets ex: <CONTENT_TO_CHANGE> -> CONTENT_CHANGED"
      :code (concat "Your response should be valid code, intended to replace the current expression in a source code file.\n"
                    "Don't use markdown code block syntax or any non-valid code in your output.")
      :fill "Complete the holes (denoted by __) in the given expression, do not change anything else!"
      :emacs (km :write-defun "Write an Emacs Lisp function with proper documentation, error handling, and optional interactive capabilities. Follow Emacs conventions for naming and structure.")
      :code-context (lambda ()
                      (km :editor "emacs"
                          :buffer-name (buffer-file-name)
                          :major-mode (symbol-name major-mode)
                          :file-content (buffer-substring-no-properties (point-min) (point-max))
                          :current-expression (pb-symex_current-as-string)))
      :task (lambda ()
              (read-string "main task: "))))

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

(seq-reduce (pb_fn [result path]
                   (km_put result path "io"))
            (mapcar #'car (km_all-paths pb-gptel_request-tree))
            ())


(defun pb-gptel_select-paths (prompt m)
  "Select paths from a map M using PROMPT with aligned annotations.
Provides completion with vertically aligned hints showing each path's content."
  (interactive)
  (let* ((flatten-tree
          (seq-reduce (pb_fn [m (cons path content)]
                             (km_put m
                                     (pb_keyword (mapconcat #'pb_keyword-name path "."))
                                     (pb_if
                                      (stringp content) (truncate-string-to-width content 100 nil nil "...")
                                      (functionp content) "#<function>"
                                      (listp content) "#<plist>"
                                      (format "%s" content))))
                      (km_all-paths m)
                      ()))
         (completion-extra-properties
          (km :affixation-function
              (lambda (candidates)
                (let ((max-len (apply #'max (mapcar #'length candidates))))
                  (mapcar (lambda (cand)
                            ;; (print cand)
                            (let ((content (km_get flatten-tree (intern cand))))
                              (list cand
                                    ""
                                    (when content
                                      (concat (make-string (- max-len (length cand) -2) ?\s)
                                              (propertize content 'face 'font-lock-comment-face))))))
                          candidates))))))
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
  (pb-gptel_request (pb-gptel_sub-request-tree)
                    (km :callback
                        (lambda (res info)
                          (let ((result-buffer (generate-new-buffer "*GPT Response*")))
                            (with-current-buffer result-buffer
                              (insert res)
                              (goto-char (point-min))
                              (when (fboundp 'org-mode)
                                (org-mode)))
                            (switch-to-buffer result-buffer))))))

(defun pb-gptel_replace-current-symex-request-callback (res info)
  (symex-change 1)
  (insert res)
  (symex-mode-interface)
  (symex-tidy))

(defun pb-gptel_current-symex-request-replace (&optional instruction)
  "Request GPT to modify the current symbolic expression based on provided instruction.
When called interactively, prompts for an instruction to guide the modification.
If INSTRUCTION is provided as an argument, uses that instead of prompting.
Uses the current expression context to send a formatted request to GPT,
and replaces the current expression with GPT's response when received."
  (interactive)
  (unless instruction
    (setq instruction (read-string "Edit current expression: ")))
  (pb-gptel_request

   (km :context
       (km :editor "emacs"
           :buffer-name (buffer-file-name)
           :major-mode (symbol-name major-mode)
           :file-content (buffer-substring (point-min) (point-max)))
       :instructions
       (km :base "You are a useful code assistant, you really like lisp-like languages and you know how to balance parentheses correctly."
           :response-format ["Your response should be valid code, intended to replace the current expression in a source code file."
                             "Don't use markdown code block syntax or any non-valid code in your output."]
           :task instruction
           :expression (pb-symex_current-as-string)))

   (km :callback
       #'pb-gptel_replace-current-symex-request-callback)))

(defun pb-gptel_fill-holes ()
  ""
  (interactive)
  (pb-gptel_current-symex-request-replace
   "Complete the holes (denoted by __) in the given expression, do not change anything else!"))


(defun pb-gptel_current-buffer-request-replace ()
  "Replace buffer contents with GPT's response to user instructions.
 Prompts for instructions, sends the current buffer as context to GPT,
 and replaces the entire buffer content with GPT's response.
 This is useful for code refactoring or complete file transformations."
  (interactive)
  (let* ((instruction (read-string "Instruction for GPT: "))
         (syntax-info (format (concat "SYNTAX_INSTRUCTIONS_START\n\n Format your response as valid code that will replace the content of CURRENT_FILE which has extension %s and is in %s mode."
                                      "\nDo not use markdown code blocks, just pure valid code for the CURRENT_FILE."
                                      "\n\nSYNTAX_INSTRUCTIONS_END")
                              (file-name-extension (buffer-file-name) t)
                              (symbol-name major-mode)))
         (file-context (concat "CURRENT_FILE_START\n\n"
                               (buffer-string)
                               "\n\nCURRENT_FILE_END\n\n"))
         (instructions (concat "\n\nMAIN_INSTRUCTIONS_START\n\n"
                               instruction
                               "\n\nMAIN_INSTRUCTIONS_END\n\n"))
         (gptel-max-tokens 10000))

    (gptel-request (concat file-context
                           "\n\n"
                           syntax-info
                           "\n\n"
                           instructions)
      :callback
      (lambda (response info)
        (when response
          (save-excursion
            (erase-buffer)
            (insert response)))))))

 (defun pb-gptel_current-buffer-request-new-buffer ()
   "Send the current buffer to GPT with user-provided instructions.
 Prompts for instructions, then sends the entire buffer as context to GPT.
 The GPT response will be displayed in a new buffer with the same major mode
 as the current buffer."
   (interactive)
   (let* ((current-mode major-mode)
          (file-extension (file-name-extension (buffer-file-name) t))
          (instruction (read-string "Instruction for GPT: "))
          (syntax-info (format (concat "SYNTAX_INSTRUCTIONS_START\n\n Format your response as valid code that matches the syntax of CURRENT_FILE which has extension %s and is in %s mode."
                                       "\nDo not use markdown code blocks, just pure valid code for the target context."
                                       "\n\nSYNTAX_INSTRUCTIONS_END")
                               file-extension
                               (symbol-name current-mode)))
          (file-context (concat "CURRENT_FILE_START\n\n"
                                (buffer-string)
                                "\n\nCURRENT_FILE_END\n\n"))
          (instructions (concat "\n\nMAIN_INSTRUCTIONS_START\n\n"
                                instruction
                                "\n\nMAIN_INSTRUCTIONS_END\n\n"))
          (result-buffer (generate-new-buffer
                          (format "GPTEL Result: %s"
                                  (file-name-base (buffer-file-name)))))
          (gptel-max-tokens 10000))
     (gptel-request (concat file-context
                            "\n\n"
                            syntax-info
                            "\n\n"
                            instructions)
       :callback
       (lambda (response info)
         (when response
           (with-current-buffer result-buffer
             (erase-buffer)
             (insert response)
             (funcall current-mode)
             (goto-char (point-min)))
           (switch-to-buffer result-buffer))))))

(provide 'pb-gptel)
;;; pb-gptel.el ends here.
