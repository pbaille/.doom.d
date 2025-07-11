
(setq pb-tools/base
      (km :eval (gptel-make-tool
                 :name "eval_elisp"
                 :function #'pb-elisp/capture-eval
                 :description (documentation #'pb-elisp/capture-eval)
                 :args (list '(:name "code"
                               :type string
                               :description "Elisp code to evaluate"))
                 :category "emacs"
                 :confirm t)

          :replace-function-definition
          (gptel-make-tool
           :name "replace_function_definition"
           :function (lambda (path start end replacement)
                       (with-current-buffer (find-file-noselect path)
                         (delete-region start end)
                         (insert replacement)))
           :description "Tool that replaces a function definition by a new implementation. Used in conjunction to the file_definitions tools (which provides start and end position)."
           :args (list '(:name "path"
                         :type string
                         :description "Path to the file containing the function")
                       '(:name "start"
                         :type integer
                         :description "Start position of the function definition to replace")
                       '(:name "end"
                         :type integer
                         :description "End position of the function definition to replace")
                       '(:name "replacement"
                         :type string
                         :description "New function definition to insert"))
           :category "buffer-editing"
           :confirm t)

          :prompt (gptel-make-tool
                   :name "prompt"
                   :description "Prompts the user for next instruction, end the chat if user input is empty."
                   :function (lambda (question)
                               (read-string (format "%s: " question)))
                   :args (list '(:name "question"
                                 :type string
                                 :description "The question to ask the user"))
                   :confirm t)))

(setq pb-tools/context
      (km :file-content (gptel-make-tool
                         :name "file-content"
                         :description "Get detailed information about a file, infos and full content."
                         :function (lambda (path)
                                     (when (file-exists-p path)
                                       (let ((file-item (pb-prompt/mk-file-item path)))
                                         (pb-prompt/mk (km :file-info file-item
                                                           :content (with-temp-buffer
                                                                      (insert-file-contents path)
                                                                      (buffer-string)))))))
                         :args (list '(:name "path"
                                       :type string
                                       :description "Path to the file to provide context for"))
                         :category "code-analysis"
                         :confirm t)

          :file-definitions (gptel-make-tool
                             :name "file-definitions"
                             :description "Extracts function and variable definitions from a file"
                             :function (lambda (path)
                                         (when (file-exists-p path)
                                           (let ((definitions (tree-browser/file-definitions path)))
                                             (if definitions
                                                 (pb-prompt/mk
                                                  (km :file path
                                                      :definitions definitions))
                                               (format "No definitions found in %s or file could not be parsed" path)))))
                             :args (list '(:name "path"
                                           :type string
                                           :description "Path to the file to extract definitions from"))
                             :category "code-analysis"
                             :confirm t)

          :directory-structure (gptel-make-tool
                                :name "directory-structure"
                                :description "Provides structure information about a directory"
                                :function (lambda (path)
                                            (when (and path (file-directory-p path))
                                              (pb-prompt/mk (pb-prompt/dir-km path))))
                                :args (list '(:name "path"
                                              :type string
                                              :description "Path to the directory to provide structure for"))
                                :category "code-analysis"
                                :confirm t)

          :project-structure (gptel-make-tool
                              :name "project-structure"
                              :description "Provides information about the current project structure"
                              :function (lambda ()
                                          (when (and (fboundp 'projectile-project-root)
                                                     (projectile-project-root))
                                            (tree-browser/dir-definitions (projectile-project-root))
                                            (pb-prompt/mk (km :project-structure
                                                              (pb-prompt/dir-km (projectile-project-root)))))
                                          "No project detected")
                              :args (list)
                              :category "code-analysis"
                              :confirm t)

          :buffer-content (gptel-make-tool
                           :name "buffer-content"
                           :description "Provides context about the current buffer or a named buffer"
                           :function (lambda (&optional buffer-name)
                                       (let ((buf (if buffer-name
                                                      (get-buffer buffer-name)
                                                    (current-buffer))))
                                         (if buf
                                             (with-current-buffer buf
                                               (pb-prompt/mk
                                                (km :buffer-info (pb-prompt/current-buffer-km)
                                                    :content (buffer-substring-no-properties (point-min) (point-max)))))
                                           (format "Buffer %s not found" buffer-name))))
                           :args (list '(:name "buffer-name"
                                         :type string
                                         :description "Name of the buffer to provide context for (defaults to current buffer)"
                                         :optional t))
                           :category "buffer-analysis"
                           :confirm t)))

(setq gptel-tools
      (append (km/vals pb-tools/base)
              (km/vals pb-tools/context)))

(pb/comment
 (qqq)
 (+lookup/definition "pb-prompt/mk")

 (find-function-noselect (intern "pb-tree"))

 (find-function-noselect (intern "pb-tree"))
 (find-function-noselect (intern "pb/pouet"))
 (defun pb/pouet ())

 (defun pb-tools/find-definition (symbol-name)
   "Find the definition of a symbol and return information about its location."
   (+lookup/definition symbol-name)
   (goto-char (point))
   (unless (looking-at "(") (forward-sexp) (backward-sexp))
   (let ((buf (current-buffer))
         (p (point)))
     (switch-to-buffer buf)
     (km :buffer buf
         :point p)))

 (+lookup--run-handler '+lookup-definition-functions
                       "pb-prompt/mk")

 (defvar pb-tools/find-definition-functions
   '(+lookup-xref-definitions-backend-fn
     +lookup-project-search-backend-fn
     +lookup-evil-goto-definition-backend-fn))

 (+lookup--xref-show 'xref-backend-definitions
                     "pb-prompt/mk3"
                     (lambda (&rest args) (print (funcall (car args))) (print (cadr args))))

 (defun pb-tools/find-definition (symbol-name)
   "Find the definition of SYMBOL-NAME and return buffer and point information.
    Uses the built-in lookup functionality to locate the definition
    and returns a km object with buffer and point information."
   (when-let* ((marker (point-marker))
               (result (run-hook-wrapped 'pb-tools/find-definition-functions
                                         #'+lookup--run-handlers
                                         symbol-name
                                         marker)))
     (km :buffer (marker-buffer result)
         :point (marker-position result))))

 (pb-tools/find-definition "pb-prompt/mk3")

 (defun pb-tools/replace-defun (name replacement)
   "Replace the definition of function NAME with REPLACEMENT.
    Uses standard Emacs functions without relying on evil or symex modes."
   (pb/let [(cons buf point) ( (intern name))]
     (with-current-buffer buf
       (save-excursion
         (goto-char point)
         (unless (looking-at "(")
           (forward-sexp))
         (pb/if [(cons beg end) (bounds-of-thing-at-point 'sexp)]
                (progn (delete-region beg end)
                       (goto-char beg)
                       (insert replacement)))))))

 (pb-tools/replace-defun "pb-tools/pouet"
                         "(defun pb-tools/pouet () \"yop yop\")")

 (pb-tools/replace-defun "pb-prompt/mk" "(defun pb-prompt/mk () \"pouet\")")

 (find-definition-noselect 'pb-prompt/mk :function)

 (pb/let [(km/keys buffer point)
          (pb-tools/find-definition "pb-prompt/current-buffer-km")]
   (switch-to-buffer buffer)
   (goto-char point))

 (+lookup/definition "pb-tools/pouet2")

 (when-let ((found (xref-find-definitions "pb-prompt/mk")))
   (with-current-buffer (marker-buffer (car (xref-item-location (car found))))
     (goto-char (marker-position (car (xref-item-location (car found)))))
     (format "Found definition in %s at position %d"
             (buffer-name)
             (point))))

 (find-function-noselect (intern "pb-tools/pouet2"))

 (defun pb-tools/pouet2 ()
   "yo")

 )
