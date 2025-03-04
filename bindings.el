;;; bindings.el -*- lexical-binding: t; -*- :emacs: :emacs:

(map!
 ;; `s-w' windows
 :desc "delete window" "s-w d" #'evil-window-delete
 :desc "kill buffer & window" "s-w C-d" #'kill-buffer-and-window
 ;; moving
 :desc "move left" "s-w h" #'windmove-left
 :desc "move right" "s-w l" #'windmove-right
 :desc "move down" "s-w j" #'windmove-down
 :desc "move up" "s-w k" #'windmove-up
 ;; splitting
 :desc "split down" "s-w C-j" (lambda () (interactive) (evil-window-split) (windmove-down))
 :desc "split right" "s-w C-l" (lambda () (interactive) (evil-window-vsplit) (windmove-right))
 :desc "split up" "s-w C-h" #'evil-window-vsplit
 :desc "split left" "s-w C-k" #'evil-window-split
 ;; swapping
 :desc "swap up" "s-w K" #'+evil/window-move-up
 :desc "swap right" "s-w L" #'+evil/window-move-right
 :desc "swap left" "s-w H" #'+evil/window-move-left
 :desc "swap down" "s-w J" #'+evil/window-move-down
 ;; where am I
 :desc "flash current position" "s-w s-w" #'+nav-flash/blink-cursor


 ;; `s-p' project
 :desc "switch persp" "s-p s-p" #'persp-switch
 :desc "project buffers" "s-p p" #'projectile-ibuffer
 :desc "find file" "s-p f" #'projectile-find-file
 :desc "recent file" "s-p r" #'projectile-recentf
 :desc "add buffer" "s-p a" #'persp-add-buffer
 :desc "search in project" "s-p s" #'+default/search-project
 :desc "kill buffer" "s-p k" #'persp-kill-buffer
 :desc "remove buffer" "s-p d" #'persp-remove-buffer
 :desc "project dired" "s-p s-d" #'project-dired
 :desc "kill presp" "s-p s-k" #'+workspace/kill
 :desc "new persp" "s-p n" #'+workspace/new-named

 ;; `s-i' info/help
 :desc "doom help menu" "s-i s-i" help-map
 :desc "describe function" "s-i f" #'helpful-callable
 :desc "describe variable" "s-i v" #'helpful-variable
 :desc "describe symbol" "s-i o" #'describe-symbol
 :desc "describe command" "s-i x" #'helpful-command
 :desc "describe key" "s-i k" #'helpful-key
 :desc "describe mode" "s-i m" #'describe-mode
 :desc "describe active minor mode" "s-i M" #'doom/describe-active-minor-mode

 ;; `s-q' LLMs
 :desc "gptel" "s-q s-q" #'gptel
 :desc "new session above" "s-q n" #'pb-gptel_new-session-above
 :desc "gptel menu" "s-q m" #'gptel-menu
 :desc "XP request insert" "s-q b i" #'pb-gptel_current-buffer-request-inlined
 :desc "XP request replace" "s-q b r" #'pb-gptel_current-buffer-request-replace
 :desc "XP request to new buffer" "s-q b b" #'pb-gptel_current-buffer-request-new-buffer)

(map! "H-C-`" #'+popup/toggle
      "s-f" #'find-file
      "s-d" #'dirvish-dwim

      "s-b" #'ibuffer
      "s-B" #'projectile-ibuffer
      "s-j" #'+vertico/switch-workspace-buffer
      "s-J" #'consult-buffer
      "s-k" #'ibuffer

      "s-C-s" #'dired-sidebar-toggle-sidebar

      "s-t" #'hs-hide-all
      "s-T" #'hs-show-all

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
      :i "C-w" evil-window-map

      ;; some extras
      :i "C-S-h" #'paredit-backward-delete
      :i "C-S-l" #'paredit-forward-delete
      ;; :i "C-w" #'pb-misc_insert-open-paren
      :i "TAB" #'consult-company)

(map! :i "s-1" #'+workspace/switch-to-0
      :i "s-2" #'+workspace/switch-to-1
      :i "s-3" #'+workspace/switch-to-2
      :i "s-4" #'+workspace/switch-to-3
      :i "s-5" #'+workspace/switch-to-4)

(map! :n "g f" #'dired-sidebar-jump-to-sidebar
      :n "g b" #'pb-ibuffer_sidebar-focus)

;; gptel TODO remove when new `s-q' is learnt
(map! :ni "s-g b" #'gptel
      :ni "s-g n" #'pb-gptel_new-session-above
      :ni "s-g g" #'gptel-menu)



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
      "t S" #'pb-dired_sidebar-reset
      "t h" #'hs-toggle-hiding
      "t R" #'rainbow-mode

      ;; buffers
      :desc "kill all dired buffers"
      "b D" #'pb-misc_kill-all-dired-buffers
      ;; exchange default doom bindings 'SPC b b' and 'SPC b B'
      :desc "consult buffer"
      "b b" #'consult-buffer
      :desc "switch worksapce buffer"
      "b B" #'+vertico/switch-workspace-buffer

      "b K" #'kill-buffer-and-window

      "o D" #'+debugger/start

      :desc "pop google search"
      "o g" (lambda () (interactive) (browse-url "https://google.com"))
      :desc "open ChatGPT 4"
      "o G" (lambda () (interactive) (browse-url "https://chat.openai.com/?model=gpt-4"))

      :desc "delete-window"
      :ni "w d" (lambda () (interactive) (delete-window))

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
            "r" #'pb-fennel_repl
            "q" #'pb-fennel_quit
            "L" #'fennel-reload
            "l" #'pb-fennel_eval-buffer
            "m" #'fennel-macroexpand
            "d" #'fennel-show-documentation
            "g" #'fennel-find-definition
            "G" #'fennel-find-module-definition
            "c" #'pb-fennel_compile-fennel
            "R" #'reapl-mode
            "I" #'pb-fennel_install-fennel-script)
      (:map reapl-mode-map
            "'" #'reapl-mode_connect
            "r" #'pb-reapl_repl
            "q" #'reapl-mode_repl-quit
            "e b" #'reapl-mode_send-buffer
            "c" #'reapl-mode_complete-symbol-at-point))

(map! (:map evil-org-mode-map
       ;; regular evil moves
       :i "C-h" #'backward-char
       :i "C-l" #'forward-char
       :i "C-j" #'evil-next-line
       :i "C-k" #'evil-previous-line
       ;; trigger sorg mode
       :ni "<return>" #'sorg--return)

      (:map gptel-mode-map
       :i "C-e" #'gptel-send
       :ni "C-m" #'gptel-menu
       "C-c C-k" #'kill-buffer-and-window)

      (:map dired-mode-map
       :nv "H" #'dired-omit-mode)

      (:map messages-buffer-mode-map
            "s-k" #'pb-misc_clear-message-buffer)

      (:map symex-mode-map
       :n "s-e" (lambda () (interactive) (symex-goto-lowest) (symex-evaluate 1))
       ;; :n "s-i" #'cider-inspect-last-result
       )

      (:map emacs-lisp-mode-map
       :i "C-p" #'pb-elisp_insert-package-prefix
       :n "C-e" #'pb-elisp_send-expression-to-ielm)

      (:map cider-inspector-mode-map
       :n "j" #'cider-inspector-next-inspectable-object
       :n "k" #'cider-inspector-previous-inspectable-object
       :n "h" #'cider-inspector-pop
       :n "l" #'cider-inspector-operate-on-point)

      (:map cider-stacktrace-mode-map
       :n [return] #'cider-stacktrace-jump)

      (:map cider-mode-map
       :n "ö" (lambda () (interactive) (save-buffer) (cider-load-buffer))
       :n "s-r" (lambda () (interactive) (save-buffer) (cider-ns-refresh))
       :n "C-s-r" (lambda () (interactive) (save-buffer) (call-interactively #'cider-jack-in-clj))
       :n "C-s-R" (lambda () (interactive) (save-buffer) (call-interactively #'cider-jack-in-cljs)))

      (:map reapl-mode-map
       :i "TAB" #'reapl-mode_complete)

      (:map fennel-repl-mode-map
            "s-r" #'other-window
            "C-k" #'comint-clear-buffer
            "C-c C-k" #'kill-buffer-and-window)

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
       :n "C-o" #'pb-dired_create-or-open-dotorg-file)

      (:map  dired-sidebar-mode-map
       :n "h" #'dired-sidebar-up-directory
       :n "l" #'pb-dired_sidebar-dwim ;#'dired-sidebar-find-file
       :n "q" #'dired-sidebar-hide-sidebar
       :n "Q" #'pb-misc_kill-all-dired-buffers
       :n "K" #'dired-subtree-up
       :n "<mouse-1>" #'pb-dired_sidebar-mouse-dwim)

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
