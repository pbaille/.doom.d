;;; bindings.el -*- lexical-binding: t; -*- :emacs: :emacs:


(map! "H-C-`" #'+popup/toggle
      "s-f" #'find-file
      "s-d" #'dired-jump
      "s-w" #'ace-window

      "s-b" #'ibuffer
      "s-B" #'projectile-ibuffer
      "s-j" #'consult-buffer
      "s-k" #'ibuffer

      "s-C-s" #'pb/toggle-sidebars

      "s-t" #'hs-hide-all
      "s-T" #'hs-show-all
      "s-M" (defun pb/display-messages () (interactive) (display-buffer "*Messages*"))

      "C-<tab>" #'company-complete
      "M-v" #'consult-yank-from-kill-ring
      "s-r" #'consult-register-load

      ;; buffer move
      "s-M-l" #'next-buffer
      "s-M-h" #'previous-buffer
      "s-<right>" #'next-buffer
      "s-<left>" #'previous-buffer

      ;; workspace move
      "s-M-C-h" #'+workspace/switch-left
      "s-M-C-l" #'+workspace/switch-right
      "s-<up>" #'+workspace/switch-left
      "s-<down>" #'+workspace/switch-right

      ;; window move
      "s-C-l" #'windmove-right
      "s-C-h" #'windmove-left
      "s-C-k" #'windmove-up
      "s-C-j" #'windmove-down
      "s-S-<right>" #'windmove-right
      "s-S-<left>" #'windmove-left
      "s-S-<up>" #'windmove-up
      "s-S-<down>" #'windmove-down)

;; vim motion in insert mode using control key.
(map! :i "C-h" #'backward-char
      :i "C-j" #'evil-next-line
      :i "C-k" #'evil-previous-line
      :i "C-l" #'forward-char

      ;; some extras
      :i "C-S-h" #'paredit-backward-delete
      :i "C-S-l" #'paredit-forward-delete
      :i "C-w" #'pb/insert-open-paren
      :i "TAB" #'consult-company)

(map! :i "s-1" #'+workspace/switch-to-0
      :i "s-2" #'+workspace/switch-to-1
      :i "s-3" #'+workspace/switch-to-2
      :i "s-4" #'+workspace/switch-to-3
      :i "s-5" #'+workspace/switch-to-4)

(map! :n "g f" #'dired-sidebar-jump-to-sidebar
      :n "g b" #'my-ibuffer-sidebar-focus)

(map! :leader

      "f e" #'consult-flycheck

      ;; overide default bookmark binding to the more useful consult-register
      "r r" #'consult-register-load
      "r f" #'consult-register
      "r s" #'consult-register-store

      "s m" #'consult-mark ; previously consult-bookmark

      "o d" #'dired-jump
      "SPC" #'consult-buffer
      "t s" #'dired-sidebar-toggle-sidebar
      "t S" #'my-dired-sidebar-reset
      "t t" #'ibuffer-sidebar-toggle-sidebar
      "t h" #'hs-toggle-hiding

      ;; buffers
      :desc "kill all dired buffers"
      "b D" #'pb/kill-all-dired-buffers
      ;; exchange default doom bindings 'SPC b b' and 'SPC b B'
      :desc "consult buffer"
      "b b" #'consult-buffer
      :desc "switch worksapce buffer"
      "b B" #'+vertico/switch-workspace-buffer

      "o D" #'+debugger/start

      :desc "pop google search"
      "o g" (lambda () (interactive) (browse-url "https://google.com"))
      :desc "open ChatGPT 4"
      "o G" (lambda () (interactive) (browse-url "https://chat.openai.com/?model=gpt-4"))

                                        ;"d d" #'org-gtd-choose
                                        ;"d c" #'org-gtd-capture
                                        ;"d e" #'org-gtd-engage
                                        ;"d p" #'org-gtd-process-inbox
                                        ;"t t" #'tab-line-mode

      )

(map! :localleader
      (:map org-mode-map
            "c p" #'org-pomodoro
            "S" #'org-insert-structure-template)
      (:map fennel-mode-map
            "r" #'fennel-repl
            "L" #'fennel-reload
            "l" #'pb/fennel-eval-buffer
            "m" #'fennel-macroexpand
            "d" #'fennel-show-documentation
            "g" #'fennel-find-definition
            "G" #'fennel-find-module-definition
            "c" #'pb/compile-fennel
            "R" #'reapl-mode
            "I" #'pb/install-fennel-script)
      (:map reapl-mode-map
            "'" #'reapl-mode_connect
            "r" #'reapl-mode_repl
            "q" #'reapl-mode_repl-quit
            "e b" #'reapl-mode_send-buffer
            "c" #'reapl-mode_complete-symbol-at-point)
      (:map company-active-map
            "C-h" #'reapl-mode_request-doc-for-completion-candidate))

(map! (:map symex-mode-map
       :n "s-e" (lambda () (interactive) (symex-goto-lowest) (symex-evaluate 1))
       :n "s-i" #'cider-inspect-last-result)

      (:map emacs-lisp-mode-map
       :i "C-p" #'pb/insert-package-prefix)

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

      (:map reaper-mode-map
       :n "s-e" #'pb/send-fnl-s-expression-to-reaper-socket-repl)

      (:map reapl-mode-map
       :i "TAB" #'reapl-mode_complete-symbol-at-point)

      (:map fennel-repl-mode-map
       :n "s-r" #'other-window
       :n "C-k" #'comint-clear-buffer)

      (:map org-mode-map
       :n "C-t" #'org-set-tags-command
       :n "g e" #'org-edit-src-code
       :n "J" #'outline-next-visible-heading
       :n "K" #'outline-previous-visible-heading
       :n "S" #'org-insert-structure-template
       :i "C-h" #'backward-char
       :i "C-j" #'evil-next-line
       :i "C-k" #'evil-previous-line
       :i "C-l" #'forward-char
       :i "C-S-h" #'paredit-backward-delete
       :i "C-S-l" #'paredit-forward-delete)

      (:map dired-mode-map
       :n "i" #'dired-subtree-toggle
       :n "h" #'dired-up-directory
       :n "l" #'dired-find-file
       :n "K" #'dired-subtree-up
       :n "s-k" #'kill-this-buffer
       :n "C-o" #'pb/dired-create-or-open-dotorg-file)

      (:map  dired-sidebar-mode-map
       :n "h" #'dired-sidebar-up-directory
       :n "l" #'my-dired-sidebar-dwim ;#'dired-sidebar-find-file
       :n "q" #'dired-sidebar-hide-sidebar
       :n "Q" #'pb/kill-all-dired-buffers
       :n "K" #'dired-subtree-up
       :n "<mouse-1>" #'my-dired-sidebar-mouse-dwim)

      (:map ibuffer-sidebar-mode-map
       :n "RET" #'my-ibuffer-sidebar-visit-buffer
       :n "l" #'my-ibuffer-sidebar-visit-buffer)

      (:map typescript-mode-map
       :ni "C-S-p" #'prettier-js
       :ni "C-S-e" #'tide-project-errors
       :ni "C-S-j" #'+lookup/definition
       :ni "C-S-k" #'+lookup/references
       :ni "C-S-h" #'tide-jump-back))

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
