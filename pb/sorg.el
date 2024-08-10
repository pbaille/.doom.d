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
  (goto-char (car (pb-org_node-bounds)))
  (sorg--flash-overlay))

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

(defun sorg--flash-overlay ()
  "Update the highlight overlay to match the start/end position of NODE."
  (interactive)
  (sorg--update-overlay)
  (run-at-time .2 nil #'sorg--delete-overlay))

;; more

(defun sorg--maybe-enter-code-block ()
  "Eventually enter org src mode.
When return is pressed on the top line of a code block during insert mode,
it enters edition mode."
  (interactive)
  (if (save-excursion (beginning-of-line) (org-at-block-p))
      (progn (org-edit-src-code)
             (evil-insert-state))
    (newline-and-indent)))

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
        "<return>" #'sorg--return
        ;; misc
        "?" #'pb-org_print-context))

(dolist (binding (sq_partition 2 2 sorg-bindings))
  (define-key evil-sorg-state-map
              (kbd (car binding))
              (cadr binding))
  (advice-add (cadr binding) :after #'sorg--flash-overlay))


(defun sorg--return ()
  "Hit return."
  (interactive)
  '(print "sorg ret")
  (cond ((evil-normal-state-p) (evil-sorg-state))
        ((evil-insert-state-p) (newline-and-indent) '(sorg--maybe-enter-code-block))
        (t (evil-sorg-state)
           (pb-org_maybe-edit-block))))

(defun sorg--click ()
  "Click mouse 1 action."
  (interactive)
  (if (evil-sorg-state-p)
      (goto-char (car (pb-org_node-bounds)))
    (call-interactively #'evil-mouse-start-end)))

(map! (:map evil-org-mode-map
       :ni "<return>" #'sorg--return
       :ni "<mouse-1>" #'sorg--click))

(progn :theming

       (setq evil-sorg-state-cursor `(box "orange"))

       (defface doom-modeline-evil-sorg-state
         '((t (:inherit doom-modeline-info)))
         "Face for the symex state tag in evil indicator."
         :group 'doom-modeline-faces)

       (defun pb-doom-modeline--sorg-modal-icon (f text face help-echo &optional icon unicode)
         "Advice around `doom-modeline--modal-icon'."
         (if (evil-sorg-state-p)
             (funcall f
                      (let ((tag (evil-state-property evil-state :tag t)))
                        (if (stringp tag) tag (funcall tag)))
                      'doom-modeline-evil-sorg-state
                      (evil-state-property evil-state :name t)
                      "nf-md-alpha_o_circle"
                      "🅞")
           (funcall f text face help-echo icon unicode)))

       (advice-add 'doom-modeline--modal-icon
                   :around
                   #'pb-doom-modeline--sorg-modal-icon))

'(:map org-src-mode-map
  :n "h" #'pb-org_go-backward)


(provide 'sorg)
;;; sorg.el ends here
