;;; bindings.el -*- lexical-binding: t; -*- :emacs: :emacs:

(map! "s-d" #'dired-jump
      "s-w" #'ace-window

      "s-b" #'ibuffer
      "s-B" #'projectile-ibuffer

      "s-t" #'hs-hide-all
      "s-T" #'hs-show-all
      "s-M" (defun pb/display-messages () (interactive) (display-buffer "*Messages*"))

      "C-<tab>" #'company-complete
      "M-v" #'consult-yank-from-kill-ring
      "s-r" #'consult-register-load

      ;; buffer move
      "C-l" #'next-buffer
      "C-h" #'previous-buffer
      "s-<right>" #'next-buffer
      "s-<left>" #'previous-buffer

      ;; workspace move
      "s-<up>" #'+workspace/switch-left
      "s-<down>" #'+workspace/switch-right

      ;; window move
      "s-S-<right>" #'windmove-right
      "s-S-<left>" #'windmove-left
      "s-S-<up>" #'windmove-up
      "s-S-<down>" #'windmove-down
      "C-M-l" #'windmove-right
      "C-M-h" #'windmove-left
      "C-M-k" #'windmove-up
      "C-M-j" #'windmove-down)


(map! :leader

      ;; overide default bookmark binding to the more useful consult-register
      "r r" #'consult-register-load
      "r f" #'consult-register
      "r s" #'consult-register-store

      "s m" #'consult-mark ; previously consult-bookmark

      "o d" #'dired-jump
      "SPC" #'dired-sidebar-jump-to-sidebar
      "t s" #'dired-sidebar-toggle-sidebar

      "b D" #'pb/kill-all-dired-buffers

      "o D" #'+debugger/start

      "o g" #'pb/open-google
      "d d" #'org-gtd-choose
      "d c" #'org-gtd-capture
      "d e" #'org-gtd-engage
      "d p" #'org-gtd-process-inbox
      "t t" #'tab-line-mode

      "t h" #'hs-toggle-hiding)

(map! :localleader
      (:map org-mode-map
            "c p" #'org-pomodoro
            "S" #'org-insert-structure-template))

(map! (:map symex-mode-map
       :n "s-e" (lambda () (interactive) (symex-goto-lowest) (symex-evaluate 1))
       :n "s-i" #'cider-inspect-last-result)

      (:map cider-inspector-mode-map
       :n "j" #'cider-inspector-next-inspectable-object
       :n "k" #'cider-inspector-previous-inspectable-object
       :n "h" #'cider-inspector-pop
       :n "l" #'cider-inspector-operate-on-point)

      (:map cider-stacktrace-mode-map
       :n [return] #'cider-stacktrace-jump)

      (:map cider-mode-map
       :n "ö" (lambda () (interactive) (save-buffer) (cider-load-buffer))
       :n "s-r" (lambda () (interactive) (save-buffer) (cider-ns-refresh)))

      (:map fennel-mode-map
       :n "SPC m R" #'pb/reaper-mode
       :n "SPC m r" #'pb/fennel-repl
       :n "SPC m l" #'pb/fennel-reload
       :n "ö" #'fennel-reload
       :n "s-r" #'pb/fennel-repl
       :n "s-R" #'pb/reaper-start-repl
       :n "M-c" #'pb/compile-fennel
       :n "M-C" #'pb/fennel-print-compile
       :n "M-i" #'pb/install-fennel-script)

      (:map reaper-mode-map
       :n "C-e" #'pb/send-fnl-s-expression-to-reaper-socket-repl)

      (:map fennel-repl-mode-map
       :n "s-r" #'other-window
       :n "C-k" #'comint-clear-buffer)

      (:map org-mode-map
       :n "C-t" #'org-set-tags-command
       :n "g e" #'org-edit-src-code
       :n "J" #'outline-next-visible-heading
       :n "K" #'outline-previous-visible-heading
       :n "S" #'org-insert-structure-template)

      (:map dired-mode-map
       :n "h" #'dired-up-directory
       :n "l" #'dired-find-file
       :n "K" #'dired-subtree-up
       :n "s-k" #'kill-this-buffer
       :n "C-o" #'pb/dired-create-or-open-dotorg-file)

      (:map  dired-sidebar-mode-map
       :n "h" #'dired-sidebar-up-directory
       :n "l" #'dired-sidebar-find-file
       :n "q" #'dired-sidebar-hide-sidebar
       :n "Q" #'pb/kill-all-dired-buffers
       :n "K" #'dired-subtree-up))

'cider-clojuredocs
'cider-doc
'cider-javadoc

'hs-hide-level
'hs-hide-level-recursive
'hs-show-block
'hs-toggle-hiding

'+snippets/new

;; bind all copilot elisp functions in an hydra bound to spc c c

'(progn :copilot
        (map! "M-c"
              #'copilot-hydra/body
              (:map symex-mode-map
               :n "C" #'copilot-hydra/body))


        ;; define the hydra with all bound copilot-* functions

        (defhydra copilot-hydra (:color pink)
          "Copilot"
          ("c" copilot-complete "complete")
          ("n" copilot-next-completion "next")
          ("a" copilot-accept-completion "accept")
          ("w" copilot-accept-completion-by-word "word")
          ("l" copilot-accept-completion-by-line "line")
          ("h" copilot-clear-overlay "clear")
          ("<return>" copilot-accept-completion "OK" :color blue)
          ("<escape>" copilot-clear-overlay "ABORT" :color blue)
          ("q" copilot-clear-overlay "quit" :color blue))

        "an hydra to test red color"
        (defhydra blue-hydra (:color blue)
          "blue"
          ("r" (message "red") "red" :color red)
          ("b" (message "b") "b")
          ("c" (message "c") "c")
          ("d" (message "d") "d"))

        "bind the hydra to M-b"
        (map! "M-b" #'blue-hydra/body))
