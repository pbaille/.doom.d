;;; pb/xp/porg.el -*- lexical-binding: t; -*-

(require 'evil)
(require 'pb-org)

(evil-define-state porg
  "Porg state."
  :tag " PORG "
  :message "-- PORG --"
  :enable (normal)
  :entry-hook (porg-enter-mode)
  :exit-hook (porg-exit-mode))

(defun porg-enter-mode ()
  ""
  (goto-char (car (pb-org_node-bounds)))
  (porg--flash-overlay))

(defun porg-exit-mode ()
  ""
  (print "exit porg"))

(setq evil-porg-state-cursor `(box "orange"))

'(:map org-src-mode-map
       :n "h" #'pb-org_go-backward)

;; overlay

(defface porg--current-node-face
  '((t :inherit symex--current-node-face :extend nil))
  "Face used to highlight the current tree node."
  :group 'porg-faces)

(defvar porg--current-overlay nil "The current overlay which highlights the current node.")

(defun porg--delete-overlay ()
  "Delete the highlight overlay."
  (when porg--current-overlay
    (delete-overlay porg--current-overlay)))

(defun porg--update-overlay ()
  "Update the highlight overlay to match the start/end position of NODE."
  (interactive)
  (porg--delete-overlay)
  (setq-local porg--current-overlay
              (let ((bounds (pb-org_node-bounds)))
                (make-overlay (car bounds) (1- (cdr bounds)))))
  (overlay-put porg--current-overlay 'face 'porg--current-node-face))

(defun porg--flash-overlay ()
  "Update the highlight overlay to match the start/end position of NODE."
  (interactive)
  (porg--update-overlay)
  (run-at-time .2 nil #'porg--delete-overlay))

(dolist (f (list #'pb-org_parent
                 #'pb-org_down-element
                 #'pb-org_backward
                 #'pb-org_forward
                 #'pb-org_last-node
                 #'pb-org_first-node
                 #'pb-org_walk-backward
                 #'pb-org_walk-forward
                 #'pb-org_move-up
                 #'pb-org_move-down
                 #'pb-org_toggle-fold
                 #'pb-org_toggle-fold
                 #'pb-org_toggle-narrow
                 #'pb-org_eval-block
                 ;; edition
                 #'pb-org_delete
                 #'pb-org_copy
                 #'pb-org_paste-after
                 #'pb-org_paste-before
                 #'pb-org_move-subtree-down
                 #'pb-org_move-subtree-up
                 #'org-insert-structure-template
                 #'pb-org_insert-after
                 #'pb-org_insert-before
                 #'pb-org_insert-at-end
                 #'pb-org_insert-at-beginning
                 #'pb-org_create-code-block
                 ;; misc
                 #'pb-org_print-context
                 ))
  (advice-add f :after #'porg--flash-overlay))

(map! (:map evil-org-mode-map
       :ni "<return>" #'pb-org_return)
      (:map evil-porg-state-map
            "k" #'pb-org_parent
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
            "o" #'pb-org_insert-after
            "O" #'pb-org_insert-before
            "a" #'pb-org_insert-at-end
            "i" #'pb-org_insert-at-beginning
            "c" #'pb-org_create-code-block
            ;; misc
            "?" #'pb-org_print-context
            ))
