;;; sorg.el --- Org evil mode inspired by Symex -*- lexical-binding: t; -*-

;; Author: Pierre Baille
;; URL: https://github.com/pbaille
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Org evil mode inspired by Symex.

;;; Code:

(require 'sq)
(require 'evil)
(require 'evil-org)
(require 'pb-org)
(require 'general)

;; mode definition

(evil-define-state sorg
  "Sorg state."
  :tag " SORG "
  :message "-- SORG --"
  :enable (normal)
  :entry-hook (sorg/enter-mode)
  :exit-hook (sorg/exit-mode))

(defun sorg/enter-mode ()
  "Run when on entering sorg mode."
  (when (eq major-mode 'org-mode)
    (setq-local pb-org/enter-src-block-function
                #'sorg/enter-lisp-block)
    (goto-char (car (pb-org/node-bounds)))
    (sorg/flash-overlay)))

(defun sorg/exit-mode ()
  "Run on exiting sorg mode."
  '(print "exit sorg"))

;; overlay

(defface sorg/current-node-face
  '((t :inherit symex--current-node-face :extend nil))
  "Face used to highlight the current tree node."
  :group 'sorg/faces)

(defvar sorg/current-overlay nil "The current overlay which highlights the current node.")

(defun sorg/delete-overlay ()
  "Delete the highlight overlay."
  (when sorg/current-overlay
    (delete-overlay sorg/current-overlay)))

(defun sorg/update-overlay ()
  "Update the highlight overlay to match the start/end position of NODE."
  (interactive)
  (sorg/delete-overlay)
  (setq-local sorg/current-overlay
              (let ((bounds (pb-org/node-bounds)))
                (make-overlay (car bounds) (1- (cdr bounds)))))
  (overlay-put sorg/current-overlay 'face 'sorg/current-node-face))

(defun sorg/flash-overlay (&rest _)
  "Update the highlight overlay to match the start/end position of NODE."
  (interactive)
  (when (eq major-mode 'org-mode)
    (sorg/update-overlay)
    (run-at-time .2 nil #'sorg/delete-overlay)
    (run-at-time .2 nil #'evil-refresh-cursor)))

;; more

(defun sorg/exit-lisp-block ()
  (let ((element (org-element-at-point)))
    (goto-char (org-element-property :begin element))
    (evil-pb-lisp-state -1)
    (evil-sorg-state 1)))

(defun sorg/enter-lisp-block ()
  (evil-sorg-state -1)
  (evil-next-line)
  (setq-local pb-lisp/escape-top-level-function #'sorg/exit-lisp-block)
  (evil-pb-lisp-state 1))

(defun sorg/enter-pb-lisp-state ()
  (evil-pb-lisp-state 1))

(defun sorg/enter-from-normal-mode ()
  (interactive)
  (cond ((and (org-in-src-block-p)
              (not (org-at-block-p)))
         (evil-pb-lisp-state 1)
         '(pb-symex/enter))

        (t (evil-sorg-state))))

(progn :pb-lisp-outside-of-blocks
       (defun sorg/pb-lisp-entry-hook-function ()
         (when (eq major-mode 'org-mode)
           (setq-local pb-lisp/enter-node-function
                       (lambda ()
                         (if (pb-org/at-lisp-block-p)
                             (progn (evil-pb-lisp-state -1)
                                    (evil-next-line)
                                    (evil-pb-lisp-state 1))
                           (pb-lisp/goto-first-child)))

                       pb-lisp/escape-top-level-function
                       (lambda ()
                         (progn (evil-pb-lisp-state -1)
                                (pb-org/code-block-goto-beg)
                                (evil-pb-lisp-state 1))))))

       (add-hook 'evil-pb-lisp-state-entry-hook
                 #'sorg/pb-lisp-entry-hook-function))

(defun sorg/click ()
  "Click mouse 1 action."
  (interactive)
  (sorg/enter-mode))

(defun sorg/scroll-top ()
  (interactive)
  (evil-scroll-line-to-top nil))

(defun sorg/goto-next-sibling-scrolling ()
  "Go to next sibling node, maintaining cursor's vertical position in window."
  (interactive)
  (let* ((win-start (window-start))
         (pos-in-win (- (point) win-start))
         (line-pos (count-screen-lines win-start (point))))
    (when (pb-org/move-down)
      ;; Adjust scroll position to keep cursor at approximately the same screen line
      (recenter line-pos))))

(defun sorg/goto-prev-sibling-scrolling ()
  "Go to previous sibling node, maintaining cursor's vertical position in window."
  (interactive)
  (let* ((win-start (window-start))
         (pos-in-win (- (point) win-start))
         (line-pos (count-screen-lines win-start (point))))
    (when (pb-org/move-up)
      ;; Adjust scroll position to keep cursor at approximately the same screen line
      (recenter line-pos))))

(defun sorg/query-replace ()
  (interactive)
  (pb-prompt/buffer-request))

(defun sorg/edit-block ()
  "Edit the source block at point."
  (interactive)
  (when (pb-org/at-code-block-p)
    (org-edit-src-code)
    (symex-mode-interface)))

(defun sorg/context-aware-return ()
  "Handle RETURN key intelligently based on context.
   - In source blocks in insert mode: create a new line with proper indentation
   - In normal insert mode: use the default org behavior"
  (interactive)
  (cond
   ;; In a source block in insert state
   ((and (org-in-src-block-p)
         (evil-insert-state-p))
    ;; Get current indentation and preserve it
    (let ((col (current-indentation)))
      (newline)
      (indent-to col)))

   ;; Default behavior - shift down one line
   (t (newline-and-indent))))

(progn :eval

       (defun sorg/eval-block (elisp-str)
         "Evaluate the Emacs Lisp string ELISP-STR and return evaluation information as a keyword map.

          The returned keyword map includes:
          - :success - Boolean indicating if evaluation completed without error
          - :result - The return value of the evaluated code
          - :output - Output captured from the message buffer
          - :error - Error message and backtrace if an error occurred
          - :duration - Time taken to evaluate the code in milliseconds

          This function carefully manages the message buffer state, restoring it after execution."
         (let ((messages-buffer (get-buffer-create "*Messages*"))
               (temp-message-buffer (get-buffer-create " *temp-messages*"))
               (backtrace-buffer (get-buffer-create "*Backtrace*"))
               (start-time (current-time))
               result success output error-msg backtrace)

           ;; Save current message buffer content
           (with-current-buffer messages-buffer
             (let ((messages-content (buffer-string)))
               (with-current-buffer temp-message-buffer
                 (erase-buffer)
                 (insert messages-content))))

           ;; Clear messages buffer (using standard function that works with read-only buffer)
           (with-current-buffer messages-buffer
             (let ((inhibit-read-only t))
               (erase-buffer)))

           ;; Clear backtrace buffer
           (with-current-buffer backtrace-buffer
             (erase-buffer))

           ;; Evaluate the code string
           (condition-case err
               (progn
                 (setq result (eval (read elisp-str)))
                 (setq success t))
             (error
              (setq success nil)
              (setq error-msg (error-message-string err))
              ;; Get backtrace if available
              (with-current-buffer backtrace-buffer
                (setq backtrace (buffer-string)))))

           ;; Capture messages output
           (with-current-buffer messages-buffer
             (setq output (buffer-string))
             ;; Restore original messages
             (let ((inhibit-read-only t))
               (erase-buffer)
               (insert-buffer-substring temp-message-buffer)))

           ;; Calculate duration
           (let ((duration (float-time (time-subtract (current-time) start-time))))
             ;; Return evaluation info as keyword map
             (km :success success
                 :result result
                 :output (when (not (string-empty-p output)) output)
                 :error (when error-msg
                          (km :message error-msg
                              :backtrace (when (not (string-empty-p backtrace)) backtrace)))
                 :duration (* 1000 duration)))))

       (defun sorg/eval-current-block ()
         "Evaluate the current org source block if it contains Emacs Lisp code.
          Returns a formatted result for display or further processing."
         (interactive)
         (if (org-in-src-block-p)
             (let* ((info (org-babel-get-src-block-info))
                    (lang (nth 0 info))
                    (body (nth 1 info))
                    (element (org-element-at-point)))
               (if (member lang '("emacs-lisp" "elisp"))
                   (let* ((result (sorg/eval-block body))
                          (block-end (org-element-property :end element))
                          (result-str (format "#+RESULTS:\n#+begin_src emacs-lisp\n%S\n#+end_src\n"
                                              result)))
                     ;; Insert result after the source block - trimming trailing newlines
                     (save-excursion
                       (goto-char block-end)
                       (skip-chars-backward "\n")
                       (insert "\n\n" result-str))
                     ;; Return the result for programmatic usage
                     result)
                 (message "Not an Emacs Lisp source block")))
           (message "Not in a source block")))

       (defun sorg/eval-and-return-result ()
         "Evaluate the current org source block and return just the result value.
          This is useful for programmatic usage where only the result is needed."
         (interactive)
         (if (org-in-src-block-p)
             (let* ((info (org-babel-get-src-block-info))
                    (lang (nth 0 info))
                    (body (nth 1 info)))
               (if (member lang '("emacs-lisp" "elisp"))
                   (km/get (sorg/eval-block body) :result)
                 (error "Not an Emacs Lisp source block")))
           (error "Not in a source block")))

       (progn :tests
              (let ((result (sorg/eval-block "(+ 1 2 3)")))
                (cl-assert (km/get result :success))
                (cl-assert (= (km/get result :result) 6))
                (cl-assert (null (km/get result :output)))
                (cl-assert (null (km/get result :error))))
              (let ((result (sorg/eval-block "(progn (message \"This is a test message\") 42)")))
                (cl-assert (km/get result :success))
                (cl-assert (= (km/get result :result) 42))
                (cl-assert (string-match "This is a test message" (km/get result :output)))
                (cl-assert (null (km/get result :error))))

              (let ((result (sorg/eval-block "(/ 1 0)")))
                (cl-assert (not (km/get result :success)))
                (cl-assert (null (km/get result :result)))
                (cl-assert (stringp (km/get (km/get result :error) :message)))
                (cl-assert (string-match "Arithmetic error" (km/get (km/get result :error) :message))))

              (let ((result (sorg/eval-block "(mapcar #'1+ '(1 2 3))")))
                (cl-assert (km/get result :success))
                (cl-assert (equal (km/get result :result) '(2 3 4)))
                (when (km/get result :success)
                  (km/get result :result)))
              :ok))

;; bindings and init

(defvar sorg/bindings
  (list "k" #'pb-org/parent
        "j" #'pb-org/down-element
        "h" #'pb-org/backward
        "l" #'pb-org/forward
        "$" #'pb-org/last-node
        "^" #'pb-org/first-node
        "b" #'pb-org/walk-backward
        "f" #'pb-org/walk-forward
        "C-k" #'pb-org/move-up
        "C-j" #'pb-org/move-down
        "C-S-k" #'sorg/goto-prev-sibling-scrolling
        "C-S-j" #'sorg/goto-next-sibling-scrolling
        "t" #'pb-org/toggle-fold
        ";" #'pb-org/toggle-fold
        "n" #'pb-org/toggle-narrow
        "e" #'sorg/eval-current-block
        "C-l" #'sorg/edit-block
        ;; edition
        "x" #'pb-org/delete
        "y" #'pb-org/copy
        "C-y" #'pb-org/copy-code-block-content
        "p" #'pb-org/paste-after
        "P" #'pb-org/paste-before
        "L" #'pb-org/move-subtree-down
        "H" #'pb-org/move-subtree-up
        "S" #'org-insert-structure-template
        "J" #'org-demote-subtree
        "K" #'org-promote-subtree
        "o" #'pb-org/insert-after
        "O" #'pb-org/insert-before
        "a" #'pb-org/insert-at-end
        "i" #'pb-org/insert-at-beginning
        "c" #'pb-org/create-code-block
        ">" #'pb-org/shift-one-line-down
        "<" #'pb-org/shift-one-line-up
        "s-l" #'sorg/enter-pb-lisp-state
        "<return>" #'pb-org/shift-one-line-down
        "S-<return>" #'pb-org/shift-one-line-up
        "<mouse-1>" #'sorg/click
        ;; misc
        "?" #'pb-org/print-context
        "g j" #'sorg/scroll-top
        "q r" #'sorg/query-replace))

(dolist (binding (sq/partition 2 2 sorg/bindings))
  (define-key evil-sorg-state-map
              (kbd (car binding))
              (cadr binding))
  ;; TODO this should be done better, without advice
  ;; (it introduced a bug related to flash overlay being called from non org buffer)
  (advice-add (cadr binding) :after #'sorg/flash-overlay))


;; Update the general-define-key binding
(general-define-key
 :states 'insert
 :keymaps (list 'evil-org-mode-map)
 [escape] #'sorg/enter-from-normal-mode
 [return] #'sorg/context-aware-return
 "C-w" #'pb-misc/insert-open-paren)

(defun sorg/after-gptel-send-advice (&rest _)
  "Return to SORG mode after sending a request to GPT from an org buffer."
  (when (eq major-mode 'org-mode)
    (run-at-time 0.1 nil
                 (lambda ()
                   (when (and (eq major-mode 'org-mode)
                              (not (evil-sorg-state-p)))
                     (evil-sorg-state 1))))))

(with-eval-after-load 'gptel
  (advice-add 'gptel-send :after #'sorg/after-gptel-send-advice))


(progn :theming

       (setq evil-sorg-state-cursor `(box "orange"))

       (defface sorg/doom-modeline-evil-state
         '((t (:inherit doom-modeline-info)))
         "Face for the symex state tag in evil indicator."
         :group 'doom-modeline-faces)

       (defun sorg/doom-modeline-modal-icon (f text face help-echo &optional icon unicode)
         "Advice around `doom-modeline--modal-icon'."
         (if (evil-sorg-state-p)
             (funcall f
                      (let ((tag (evil-state-property evil-state :tag t)))
                        (if (stringp tag) tag (funcall tag)))
                      'sorg/doom-modeline-evil-state
                      (evil-state-property evil-state :name t)
                      "nf-md-alpha_o_circle"
                      "🅞")
           (funcall f text face help-echo icon unicode)))

       (advice-add 'doom-modeline--modal-icon
                   :around
                   #'sorg/doom-modeline-modal-icon))

(progn :request

       (defun pb-gptel/current-sorg-request-handler (res info)
         "Replace current org node with GPT model response.
          This function handles the response from a GPT model request and replaces
          the current org node with that response.

          The function performs the following steps:
          1. Delete the content of the current org node
          2. Insert the model's response text in place of the original content
          3. Ensure proper formatting and indentation of the edited node

          Parameters:
          - RES: The response text from the language model
          - INFO: A plist containing metadata about the request (provided by gptel)"

         ;; Get the current node bounds
         (let* ((bounds (pb-org/node-bounds))
                (start (car bounds))
                (end (cdr bounds)))

           ;; Delete the current node content
           (delete-region start end)

           ;; Insert the response or show error message
           (if res
               (progn
                 (goto-char start)
                 (insert res))
             (message (km/pp (km :status (km/get info :http-status)
                                 :error (km/get info :error)))))

           ;; Update the overlay to highlight the new content
           (sorg/flash-overlay)

           ;; Clean up any formatting issues
           (when (eq major-mode 'org-mode)
             (org-indent-region start (point))
             (org-fix-tags-on-the-fly)))))

(provide 'sorg)
;;; sorg.el ends here
