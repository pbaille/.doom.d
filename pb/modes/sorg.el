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
  :entry-hook (sorg-enter-mode)
  :exit-hook (sorg-exit-mode))

(defun sorg-enter-mode ()
  "Run when on entering sorg mode."
  (when (eq major-mode 'org-mode)
    (setq-local pb-org/enter-src-block-function
                #'sorg--enter-lisp-block)
    (goto-char (car (pb-org/node-bounds)))
    (sorg--flash-overlay)))

(defun sorg-exit-mode ()
  "Run on exiting sorg mode."
  '(print "exit sorg"))

;; overlay

(defface sorg--current-node-face
  '((t :inherit symex--current-node-face :extend nil))
  "Face used to highlight the current tree node."
  :group 'sorg-faces)

(defvar sorg--current-overlay nil "The current overlay which highlights the current node.")

(defun sorg--delete-overlay ()
  "Delete the highlight overlay."
  (when sorg--current-overlay
    (delete-overlay sorg--current-overlay)))

(defun sorg--update-overlay ()
  "Update the highlight overlay to match the start/end position of NODE."
  (interactive)
  (sorg--delete-overlay)
  (setq-local sorg--current-overlay
              (let ((bounds (pb-org/node-bounds)))
                (make-overlay (car bounds) (1- (cdr bounds)))))
  (overlay-put sorg--current-overlay 'face 'sorg--current-node-face))

(defun sorg--flash-overlay (&rest _)
  "Update the highlight overlay to match the start/end position of NODE."
  (interactive)
  (when (eq major-mode 'org-mode)
    (sorg--update-overlay)
    (run-at-time .2 nil #'sorg--delete-overlay)
    (run-at-time .2 nil #'evil-refresh-cursor)))

;; more

(defun sorg--exit-lisp-block ()
  (let ((element (org-element-at-point)))
    (goto-char (org-element-property :begin element))
    (evil-pb-lisp-state -1)
    (evil-sorg-state 1)))

(defun sorg--enter-lisp-block ()
  (evil-sorg-state -1)
  (evil-next-line)
  (setq-local pb-lisp/escape-top-level-function #'sorg--exit-lisp-block)
  (evil-pb-lisp-state 1))

(defun sorg--enter-pb-lisp-state ()
  (evil-pb-lisp-state 1))

(defun sorg--enter-from-normal-mode ()
  (interactive)
  (cond ((and (org-in-src-block-p)
              (not (org-at-block-p)))
         (evil-pb-lisp-state 1)
         '(pb-symex/enter))

        (t (evil-sorg-state))))

(progn :pb-lisp-outside-of-blocks
       (defun sorg--pb-lisp-entry-hook-function ()
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
                 #'sorg--pb-lisp-entry-hook-function))

(defun sorg--click ()
  "Click mouse 1 action."
  (interactive)
  (sorg-enter-mode))

(defun sorg--scroll-top ()
  (interactive)
  (evil-scroll-line-to-top nil))

(defun sorg--goto-next-sibling-scrolling ()
  "Go to next sibling node, maintaining cursor's vertical position in window."
  (interactive)
  (let* ((win-start (window-start))
         (pos-in-win (- (point) win-start))
         (line-pos (count-screen-lines win-start (point))))
    (when (pb-org/move-down)
      ;; Adjust scroll position to keep cursor at approximately the same screen line
      (recenter line-pos))))

(defun sorg--goto-prev-sibling-scrolling ()
  "Go to previous sibling node, maintaining cursor's vertical position in window."
  (interactive)
  (let* ((win-start (window-start))
         (pos-in-win (- (point) win-start))
         (line-pos (count-screen-lines win-start (point))))
    (when (pb-org/move-up)
      ;; Adjust scroll position to keep cursor at approximately the same screen line
      (recenter line-pos))))

(defun sorg--query-replace ()
  (interactive)
  (pb-prompt/buffer-request))

(defun sorg--edit-block ()
  "Edit the source block at point."
  (interactive)
  (when (pb-org/at-code-block-p)
    (org-edit-src-code)
    (symex-mode-interface)))

(defun sorg--context-aware-return ()
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
   (t (pb-org/shift-one-line-down))))

;; bindings and init

(defvar sorg-bindings
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
        "C-S-k" #'sorg--goto-prev-sibling-scrolling
        "C-S-j" #'sorg--goto-next-sibling-scrolling
        "t" #'pb-org/toggle-fold
        ";" #'pb-org/toggle-fold
        "n" #'pb-org/toggle-narrow
        "e" #'pb-org/eval-block
        "C-l" #'sorg--edit-block
        ;; edition
        "x" #'pb-org/delete
        "y" #'pb-org/copy
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
        "s-l" #'sorg--enter-pb-lisp-state
        "<return>" #'pb-org/shift-one-line-down
        "S-<return>" #'pb-org/shift-one-line-up
        "<mouse-1>" #'sorg--click
        ;; misc
        "?" #'pb-org/print-context
        "g j" #'sorg--scroll-top
        "q r" #'sorg--query-replace))

(dolist (binding (sq/partition 2 2 sorg-bindings))
  (define-key evil-sorg-state-map
              (kbd (car binding))
              (cadr binding))
  ;; TODO this should be done better, without advice
  ;; (it introduced a bug related to flash overlay being called from non org buffer)
  (advice-add (cadr binding) :after #'sorg--flash-overlay))


;; Update the general-define-key binding
(general-define-key
 :states 'insert
 :keymaps (list 'evil-org-mode-map)
 [escape] #'sorg--enter-from-normal-mode
 [return] #'sorg--context-aware-return
 "C-w" #'pb-misc/insert-open-paren)

(defun sorg--after-gptel-send-advice (&rest _)
  "Return to SORG mode after sending a request to GPT from an org buffer."
  (when (eq major-mode 'org-mode)
    (run-at-time 0.1 nil
                 (lambda ()
                   (when (and (eq major-mode 'org-mode)
                              (not (evil-sorg-state-p)))
                     (evil-sorg-state 1))))))

(with-eval-after-load 'gptel
  (advice-add 'gptel-send :after #'sorg--after-gptel-send-advice))


(progn :theming

       (setq evil-sorg-state-cursor `(box "orange"))

       (defface sorg-doom-modeline-evil-state
         '((t (:inherit doom-modeline-info)))
         "Face for the symex state tag in evil indicator."
         :group 'doom-modeline-faces)

       (defun sorg-doom-modeline-modal-icon (f text face help-echo &optional icon unicode)
         "Advice around `doom-modeline--modal-icon'."
         (if (evil-sorg-state-p)
             (funcall f
                      (let ((tag (evil-state-property evil-state :tag t)))
                        (if (stringp tag) tag (funcall tag)))
                      'sorg-doom-modeline-evil-state
                      (evil-state-property evil-state :name t)
                      "nf-md-alpha_o_circle"
                      "🅞")
           (funcall f text face help-echo icon unicode)))

       (advice-add 'doom-modeline--modal-icon
                   :around
                   #'sorg-doom-modeline-modal-icon))

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
           (sorg--flash-overlay)

           ;; Clean up any formatting issues
           (when (eq major-mode 'org-mode)
             (org-indent-region start (point))
             (org-fix-tags-on-the-fly)))))

(provide 'sorg)
;;; sorg.el ends here
