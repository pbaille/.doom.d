;;; pb/my-org.el -*- lexical-binding: t; -*-

;; Utility belt for manipulating org files
;; The initial motivation is to build a system for adding metadata to files and dirs

;; olp stands for outline path
;; this switches to the scratch org buffer and go to one of its sub header
(progn (switch-to-buffer "scratch.org")
       (goto-char (org-find-olp (list "~/org/scratch.org" "top" "three" "3.2"))))

(defun my-org-find-or-create-olp-aux (file current-marker current-path remaining-path)
  (print (list "-" current-marker current-path remaining-path))
  (or (let ((next-marker (org-find-olp (cons file current-path))))
        (print next-marker)
        (if next-marker
            (if remaining-path
                (my-org-find-or-create-olp-aux file
                                               current-marker
                                               (append current-path (list (car remaining-path)))
                                               (cdr remaining-path))
              (list
               :file file
               :at current-path
               :to-create remaining-path
               :mark next-marker))))
      (list
       :file file
       :at current-path
       :to-create remaining-path
       :mark current-marker)))

(defun my-org-find-or-create-olp (file path)
  (my-org-find-or-create-olp-aux file 0 (list (car path)) (cdr path)))

(my-org-find-or-create-olp "~/org/scratch.org" (list "top" "tao"))
