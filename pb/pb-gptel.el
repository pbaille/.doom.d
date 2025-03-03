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

 (defun pb-gptel_replace-current-symex-request-callback (res info)
   (symex-change 1)
   (insert res)
   (symex-mode-interface)
   (symex-tidy))

 (defun pb-gptel_fill-holes ()
   ""
   (interactive)
   (pb-gptel_request

    (km :context
        (km :editor "emacs"
            :buffer-name (buffer-file-name)
            :major-mode (symbol-name major-mode)
            :file-content (buffer-substring (point-min) (point-max)))
        :instructions
        (km :base "You are a usefull code assistant, you really like lisp like languages and you know how to balance parenthesis correctly."
            :response-format ["Your response should be valid code, intended to be inserted in source code file."
                              "Don't use markdown code block syntax or any non valid code in your output."]
            :task "Complete the holes (denoted by __) in the given expression, do not change anything else!"
            :expression (pb-symex_current-as-string)))

    (km :callback
        #'pb-gptel_replace-current-symex-request-callback)))

 (defun pb-gptel_current-symex-request-replace ()
   ""
   (interactive)
   (let ((instruction (read-string "Edit current expression: ")))
     (pb-gptel_request

      (km :context
          (km :editor "emacs"
              :buffer-name (buffer-file-name)
              :major-mode (symbol-name major-mode)
              :file-content (buffer-substring (point-min) (point-max)))
          :instructions
          (km :base "You are a usefull code assistant, you really like lisp like languages and you know how to balance parenthesis correctly."
              :response-format ["Your response should be valid code, intended to replace the current expression in source code file."
                                "Don't use markdown code block syntax or any non valid code in your output."]
              :task instruction
              :expression (pb-symex_current-as-string)))

      (km :callback
          #'pb-gptel_replace-current-symex-request-callback))))

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
