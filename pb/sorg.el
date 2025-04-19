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
    (goto-char (car (pb-org_node-bounds)))
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
              (let ((bounds (pb-org_node-bounds)))
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
         '(pb-symex_enter))

        (t (evil-sorg-state))))

(progn :pb-lisp-outside-of-blocks
       (defun sorg--pb-lisp-entry-hook-function ()
         (when (eq major-mode 'org-mode)
           (setq-local pb-lisp/enter-node-function
                       (lambda ()
                         (if (pb-org_at-lisp-block-p)
                             (progn (evil-pb-lisp-state -1)
                                    (evil-next-line)
                                    (evil-pb-lisp-state 1))
                           (pb-lisp/goto-first-child)))

                       pb-lisp/escape-top-level-function
                       (lambda ()
                         (progn (evil-pb-lisp-state -1)
                                (pb-org_code-block-goto-beg)
                                (evil-pb-lisp-state 1))))))

       (add-hook 'evil-pb-lisp-state-entry-hook
                 #'sorg--pb-lisp-entry-hook-function))

(defun sorg--click ()
  "Click mouse 1 action."
  (interactive)
  (sorg-enter-mode))

;; bindings and init

(defvar sorg-bindings
  (list "k" #'pb-org_parent
        "j" #'pb-org_down-element
        "h" #'pb-org_backward
        "l" #'pb-org_forward
        "$" #'pb-org_last-node
        "^" #'pb-org_first-node
        "b" #'pb-org_walk-backward
        "f" #'pb-org_walk-forward
        "C-k" #'pb-org_move-up
        "C-j" #'pb-org_move-down
        "t" #'pb-org_toggle-fold
        ";" #'pb-org_toggle-fold
        "n" #'pb-org_toggle-narrow
        "e" #'pb-org_eval-block
        ;; edition
        "x" #'pb-org_delete
        "y" #'pb-org_copy
        "p" #'pb-org_paste-after
        "P" #'pb-org_paste-before
        "L" #'pb-org_move-subtree-down
        "H" #'pb-org_move-subtree-up
        "S" #'org-insert-structure-template
        "J" #'org-demote-subtree
        "K" #'org-promote-subtree
        "o" #'pb-org_insert-after
        "O" #'pb-org_insert-before
        "a" #'pb-org_insert-at-end
        "i" #'pb-org_insert-at-beginning
        "c" #'pb-org_create-code-block
        ">" #'pb-org_shift-one-line-down
        "<" #'pb-org_shift-one-line-up
        "s-l" #'sorg--enter-pb-lisp-state
        "<return>" #'pb-org_shift-one-line-down
        "S-<return>" #'pb-org_shift-one-line-up
        "<mouse-1>" #'sorg--click
        ;; misc
        "?" #'pb-org_print-context))

(dolist (binding (sq_partition 2 2 sorg-bindings))
  (define-key evil-sorg-state-map
              (kbd (car binding))
              (cadr binding))
  ;; TODO this should be done better, without advice
  ;; (it introduced a bug related to flash overlay being called from non org buffer)
  (advice-add (cadr binding) :after #'sorg--flash-overlay))


(general-define-key
 :states 'insert
 :keymaps (list 'evil-org-mode-map)
 [escape] #'sorg--enter-from-normal-mode
 "C-w" #'pb-misc_insert-open-paren)

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

(provide 'sorg)
;;; sorg.el ends here
