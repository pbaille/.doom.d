;;; bindings.el -*- lexical-binding: t; -*- :emacs: :emacs:

(setq which-key-sort-order 'which-key-description-order)

(progn :main

       ;; impossible to override s-w d, this commands closes the sidebar, which is annoying,
       ;; I've tried many things but nothing worked, had to be brutal...
       (advice-add '+workspace/close-window-or-workspace :override #'delete-window)

       (map! (:prefix ("s-w" . "windows")
                      (:desc "delete window" "s-d" (lambda () (interactive) (delete-window)))
                      (:desc "quit window" "q" #'quit-window)
                      (:desc "kill buffer & window" "C-d" (lambda () (interactive) (kill-buffer-and-window)))

                      ;; moving
                      (:desc "move left" "h" #'windmove-left)
                      (:desc "move right" "l" #'windmove-right)
                      (:desc "move down" "j" #'windmove-down)
                      (:desc "move up" "k" #'windmove-up)

                      ;; resizing
                      (:desc "width +" "L" #'pb-misc/increase-window-width)
                      (:desc "width -" "H" #'pb-misc/shrink-window-width)
                      (:desc "height +" "J" #'pb-misc/increase-window-height)
                      (:desc "height -" "K" #'pb-misc/shrink-window-height)
                      (:desc "balance" "TAB" #'balance-windows)

                      ;; splitting
                      (:desc "split down" "C-j" (lambda () (interactive) (evil-window-split) (windmove-down)))
                      (:desc "split right" "C-l" (lambda () (interactive) (evil-window-vsplit) (windmove-right)))
                      (:desc "split up" "C-h" #'evil-window-vsplit)
                      (:desc "split left" "C-k" #'evil-window-split)

                      ;; swapping
                      (:desc "swap up" "M-k" #'+evil/window-move-up)
                      (:desc "swap right" "M-l" #'+evil/window-move-right)
                      (:desc "swap left" "M-h" #'+evil/window-move-left)
                      (:desc "swap down" "M-j" #'+evil/window-move-down)

                      ;; misc
                      (:desc "flash current position" "s-w" #'+nav-flash/blink-cursor)
                      (:desc "new window" "n" #'pb-misc/dwim-split)
                      (:desc "split open buffer" "b" #'pb-misc/window-split-consult-buffer)
                      (:desc "delete other windows" "D" #'delete-other-windows)))

       (map! (:prefix ("s-p" . "project")

                      (:desc "switch persp"
                             "s-p" #'persp-switch)

                      (:desc "project buffers"
                             "p" #'projectile-ibuffer)

                      (:desc "find file"
                             "f" #'projectile-find-file)

                      (:desc "recent file"
                             "r" #'projectile-recentf)

                      (:desc "add buffer"
                             "a" #'persp-add-buffer)

                      (:desc "search in project"
                             "s" #'+default/search-project)

                      (:desc "kill buffer"
                             "k" #'persp-kill-buffer)

                      (:desc "remove buffer"
                             "d" #'persp-remove-buffer)

                      (:desc "project dired"
                             "s-d" #'project-dired)

                      (:desc "kill persp"
                             "s-k" #'+workspace/kill)

                      (:desc "new persp"
                             "n" #'+workspace/new-named)

                      (:desc "next persp"
                             "l" #'+workspace/switch-right)

                      (:desc "prev persp"
                             "h" #'+workspace/switch-left)

                      (:desc "move right"
                             "M-l" #'+workspace/swap-right)

                      (:desc "move left"
                             "M-h" #'+workspace/swap-left)))

       (map! (:prefix ("s-o" . "open")
                      (:desc "vterm here" "t" #'+vterm/here)
                      (:desc "project sidebar" "s-p" #'+dired/dirvish-side-and-follow)))

       (map! (:prefix ("s-i" . "info/help")
                      (:desc "doom help menu" "h" help-map)
                      (:desc "imenu" "s-i" #'consult-imenu)
                      (:desc "imenu quit" "q" #'tree-browser/quit)
                      (:desc "describe function" "f" #'helpful-callable)
                      (:desc "describe face" "F" #'describe-face)
                      (:desc "describe variable" "v" #'helpful-variable)
                      (:desc "describe symbol" "o" #'describe-symbol)
                      (:desc "describe command" "x" #'helpful-command)
                      (:desc "describe key" "k" #'helpful-key)
                      (:desc "describe mode" "m" #'describe-mode)
                      (:desc "describe active minor mode" "M" #'doom/describe-active-minor-mode)))

       (map! (:prefix ("s-j" . "buffers")

                      (:desc "project buffer"
                             "s-j" #'consult-project-buffer)

                      (:desc "consult buffer"
                             "j" #'consult-buffer)

                      (:desc "ibuffer"
                             "s-i" #'ibuffer)

                      (:desc "*messages*"
                             "M" #'pb-misc/switch-to-message-buffer)

                      (:desc "meta buffers"
                             "m" #'pb-misc/switch-to-meta-buffer)

                      (:desc "terminals"
                             "t" #'pb-misc/select-vterm-buffer)

                      (:desc "project ibuffer"
                             "i" #'projectile-ibuffer)

                      (:desc "kill buffer"
                             "k" #'kill-current-buffer)

                      (:desc "kill buffer and window"
                             "s-k" #'kill-buffer-and-window)

                      (:desc "reload buffer"
                             "r" #'revert-buffer)

                      (:desc "rename buffer"
                             "R" #'rename-buffer)

                      (:desc "consult flycheck"
                             "w" #'consult-flycheck)

                      (:desc "scratch buffer"
                             "s-n" #'pb-misc/scratch-buffer)

                      (:desc "scratch buffer"
                             "n" (lambda ()
                                   (interactive)
                                   (pb-misc/scratch-buffer 'split)))

                      (:desc "new buffer"
                             "N" #'pb-misc/new-buffer)

                      (:desc "next"
                             "l" #'next-buffer)

                      (:desc "previous"
                             "h" #'previous-buffer)

                      (:desc "elisp eval" "C-e" (lambda ()
                                                  (interactive)
                                                  (pb-misc/elisp-eval 'split)))))

       (map! (:prefix ("s-q" . "LLMs")

                      ;; gptel

                      (:desc "gptel: goto chat"
                             "s-q" #'gptel)

                      (:desc "gptel: menu"
                             "m" #'gptel-menu)

                      (:desc "gptel: change model"
                             "s-m" #'pb-gptel/change-model)

                      (:desc "gptel: add file to context"
                             "f" #'gptel-context-add-file)

                      (:desc "gptel: remove all context"
                             "D" #'gptel-context-remove-all)

                      (:desc "gptel: remove files from context"
                             "d" #'pb-gptel/remove-context-files)

                      (:prefix ("t" . "gptel: tools")
                               (:desc "enable" "e" (lambda () (interactive) (setq-local gptel-use-tools t)))
                               (:desc "disable" "d" (lambda () (interactive) (setq-local gptel-use-tools nil))))

                      ;; chat

                      (:desc "chat: current expression"
                             "e" #'pb-gptel/current-symex-chat)

                      (:desc "chat: current buffer"
                             "b" #'pb-gptel/current-buffer-chat)

                      (:desc "chat: current buffer (new window)"
                             "s-b" (lambda () (interactive) (pb-gptel/current-buffer-chat (km :pop-window t))))

                      (:desc "chat: directory"
                             "s-d" #'pb-gptel/directory-chat)

                      ;; git
                      (:prefix ("g" . "git:")
                               (:desc "generate commit message" "m" #'pb-prompt/generate-commit-message))))

       (map! (:prefix ("s-c" . "prompt: context")
                      (:desc "display" "C-e" #'pb-prompt/display-context)
                      (:desc "local context" "s-c" #'pb-prompt/browse-context-file)
                      (:prefix ("a" . "add")
                               (:desc "add buffer to context" "b" #'pb-prompt/add-buffer)
                               (:desc "add file to context" "f" #'pb-prompt/add-path)
                               (:desc "add project file or dir" "p" #'pb-prompt/add-project-file-or-dir)
                               (:desc "add function to context" "l" #'pb-prompt/add-function)
                               (:desc "add selection to context" "s" #'pb-prompt/add-selection)
                               (:desc "add saved context" "c" #'pb-prompt/add-saved-context))

                      (:desc "delete item" "d" #'pb-prompt/remove-context-item)

                      (:desc "save" "s" #'pb-prompt/save-context)
                      (:desc "load" "l" #'pb-prompt/load-context)
                      (:prefix ("m" . "meta")
                               (:desc "load" "l" #'pb-meta/load-meta-context)
                               (:desc "save" "s" #'pb-meta/create-context-file))))

       (map! (:prefix ("s-g" . "git")
                      (:desc "git status" "s-g" #'magit-status)
                      (:prefix ("d" . "diff")
                               (:desc "diff file" "f" #'magit-diff-buffer-file)
                               (:desc "diff root" "s" #'magit-diff-staged)
                               (:desc "diff root" "b" #'pb-prompt/diff-branch))
                      (:prefix ("c" . "commit")
                               (:desc "commit" "c" #'pb-prompt/commit)
                               (:desc "amend" "a" #'pb-prompt/commit-amend))
                      (:desc "save & stage" "s" #'pb-git/stage-file)
                      (:desc "save, stage, commit" "s-s" (lambda () (interactive) (pb-git/stage-file) (pb-prompt/commit)))))

       (map! (:prefix ("s-s" . "search")
                      (:desc "search file" "s-s" #'+default/search-buffer)
                      (:desc "search symbol at point" "s" #'+vertico/search-symbol-at-point)
                      (:desc "search project" "p" #'+default/search-project)
                      (:desc "search replace thing at point" "r" #'pb-misc/query-replace-thing-at-point)
                      (:desc "search replace" "s-r" #'query-replace)
                      (:desc "search google" "g" (lambda () (interactive) (browse-url "https://www.google.com/?autofocus=1")))
                      (:desc "search mark" "m" #'consult-mark)
                      (:desc "search symbol" "e" #'pb-misc/search-symbol)
                      (:desc "search symbol" "s-e" (lambda () (interactive) (pb-misc/search-symbol :refresh)))))

       (map! (:prefix ("s-f" . "file")
                      (:desc "find file" "s-f" #'find-file)
                      (:desc "recent file" "r" #'consult-recent-file)
                      (:desc "copy file" "C" #'doom/copy-this-file)
                      (:desc "move file" "R" #'doom/move-this-file)
                      (:desc "find project file" "p" #'projectile-find-file)
                      (:desc "save file" "s-s" #'pb-misc/save-buffer)
                      (:desc "delete file" "D" #'doom/delete-this-file)))

       (map! (:prefix ("s-d" . "dired")
                      (:desc "dired" "s-d" (lambda ()
                                             (interactive)
                                             (dired-jump)
                                             (dired-omit-mode 1)
                                             ;; :( conflicting hook probably...
                                             (run-at-time 0.01 nil (lambda () (dired-hide-details-mode 1)))))
                      (:desc "dirvish" "d" (lambda ()
                                             (interactive)
                                             (let ((file buffer-file-name))
                                               (dirvish (file-name-directory file))
                                               (dirvish-find-entry-a file))))
                      (:desc "sidebar" "s" #'dired-sidebar-toggle-sidebar)
                      (:desc "dired kill all" "s-k" #'pb-misc/kill-all-dired-buffers)))

       (map! (:prefix ("s-t" . "toggle")
                      (:desc "folding" "s-t" #'pb-fold/toggle-fold)
                      (:desc "fold level" "t l" #'hs-hide-level)
                      (:desc "fold show block" "t o" #'hs-show-block)
                      (:desc "flycheck" "f" #'flycheck-mode)
                      (:desc "line numbers" "l" #'doom/toggle-line-numbers)
                      (:desc "highlight line" "h" #'hl-line-mode)
                      (:desc "word wrap" "w" #'visual-line-mode)
                      (:desc "read only" "r" #'read-only-mode)))

       (map! (:prefix ("s-m" . "meta")
                      (:desc "find meta file" "s-m" #'pb-meta/change-or-create-meta-file)
                      (:desc "find project meta file" "p" #'pb-meta/find-all-meta-files)
                      (:desc "goto main file" "m" #'pb-meta/goto-main-file)
                      (:prefix ("n" . "create")
                               (:desc "org file" "o" #'pb-meta/create-org-file)
                               (:desc "scratch file" "s" #'pb-meta/create-scratch-file)))))

(progn :misc

       (map! :ni "C-h" #'backward-char
             :ni "C-j" #'evil-next-line
             :ni "C-k" #'evil-previous-line
             :ni "C-l" #'forward-char
             :n "M-C-j" (lambda () (interactive) (evil-insert-newline-above) (evil-next-line))
             :n "M-C-k" (lambda () (interactive) (delete-indentation))

             ;; some extras
             :i "C-S-h" #'paredit-backward-delete
             :i "C-S-l" #'paredit-forward-delete
             ;; :i "C-w" #'pb-misc/insert-open-paren
             :i "TAB" #'consult-company
             :i "M-v" #'consult-yank-from-kill-ring
             :i "s-c" #'copy-region-as-kill)

       (map! :v "s-x" #'execute-extended-command)

       (map! :i "s-1" #'+workspace/switch-to-0
             :i "s-2" #'+workspace/switch-to-1
             :i "s-3" #'+workspace/switch-to-2
             :i "s-4" #'+workspace/switch-to-3
             :i "s-5" #'+workspace/switch-to-4)

       (map! :n "g f" #'dired-sidebar-jump-to-sidebar
             :n "g b" #'pb-ibuffer/sidebar-focus)

       (map! :n "g j" #'evil-scroll-line-to-top
             :n "g k" #'evil-scroll-line-to-bottom
             :n "g C-h" #'pb-misc/scroll-to-leftmost))

(map! :leader

      ;; overide default bookmark binding to the more useful consult-register
      "r r" #'consult-register-load
      "r f" #'consult-register
      "r s" #'consult-register-store

      "o D" #'+debugger/start)

(map! :localleader
      (:map org-mode-map
            "c p" #'org-pomodoro
            "S" #'org-insert-structure-template)
      (:map fennel-mode-map
            "r" #'pb-fennel/repl
            "q" #'pb-fennel/quit
            "L" #'fennel-reload
            "l" #'pb-fennel/eval-buffer
            "m" #'fennel-macroexpand
            "d" #'fennel-show-documentation
            "g" #'fennel-find-definition
            "G" #'fennel-find-module-definition
            "c" #'pb-fennel/compile-fennel
            "R" #'reapl-mode
            "I" #'pb-fennel/install-fennel-script)
      (:map reapl-mode-map
            "'" #'reapl-mode_connect
            "r" #'pb-reapl/repl
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
       :n "<return>" #'sorg/enter-from-normal-mode
       :ni "s-l" #'symex-mode-interface
       :n "<mouse-1>" (lambda () (interactive) (evil-normal-state 1))
       :n "s-i s-i" #'tree-browser/navigate-buffer
       :n "s-s s-i" (lambda () (interactive) (tree-browser/navigate-buffer) (tree-browser/live-search)))

      (:map gptel-mode-map
       :i "C-e" #'gptel-send
       :ni "C-m" #'gptel-menu
       "C-c C-k" #'kill-buffer-and-window)

      (:map dired-mode-map
       :nv "H" #'dired-omit-mode
       :nv "m" #'dired-mark;; #'pb-meta/change-or-create-meta-file
       :nv "C-d" #'dired-hide-details-mode)

      (:map messages-buffer-mode-map
            "s-k" #'pb-misc/clear-message-buffer)

      (:map symex-mode-map
       :n "s-e" (lambda () (interactive) (symex-goto-lowest) (symex-evaluate 1))
       ;; :n "s-i" #'cider-inspect-last-result
       )

      (:map emacs-lisp-mode-map
       :i "C-p" #'pb-elisp/insert-package-prefix
       :n "s-j e" #'eval-buffer
       :n "s-j b" #'tree-browser/navigate-buffer
       :n "s-l" #'evil-pb-lisp-state
       :n "s-i s-i" #'tree-browser/navigate-buffer
       :n "s-s s-i" (lambda () (interactive) (tree-browser/navigate-buffer) (tree-browser/live-search)))

      (:map clojure-mode-map
       :n "s-l" #'evil-pb-lisp-state)

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
       :n "C-s-R" (lambda () (interactive) (save-buffer) (call-interactively #'cider-jack-in-cljs))
       :desc "select repl" "s-j r" #'pb-cider/select-repl-buffer
       :desc "eval buffer" "s-j e" #'cider-eval-buffer)

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
       :n "s-k" #'kill-this-buffer)

      (:map  dired-sidebar-mode-map
       :n "h" #'dired-sidebar-up-directory
       :n "l" #'pb-dired/sidebar-dwim ;#'dired-sidebar-find-file
       :n "q" #'dired-sidebar-hide-sidebar
       :n "<return>" #'pb-dired/sidebar-dwim
       :n "Q" #'pb-misc/kill-all-dired-buffers
       :n "K" #'dired-subtree-up
       :n "<mouse-1>" #'pb-dired/sidebar-mouse-dwim)

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
