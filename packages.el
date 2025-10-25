;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.

(package! cider)
(package! clojure-mode)

(package! pb
  :recipe (:local-repo "/Users/pierrebaille/.doom.d/pb"
           :files ("*.el" "modes/*.el" "base/*.el" "languages/*.el" "llms/*.el" "utils/*.el" "reaper/*.el")))

;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:

;; colors
(package! info-colors)
(package! command-log-mode)
(package! webkit-color-picker)
(package! kurecolor)

(package! consult-company)

;; ui
(package! auto-dim-other-buffers)
(package! ef-themes
  :recipe (:local-repo "/Users/pierrebaille/.doom.d/themes/ef-themes"
           :files ("*.el")))
(package! modus-themes
  :recipe (:local-repo "/Users/pierrebaille/Code/forks/modus-themes"
           :files ("*.el")))
(package! dired-sidebar)
;; (package! nerd-icons-dired)
(package! spacious-padding)
(package! nerd-icons-ibuffer) ;; replace keywords or regular expression with SVG tags.

;; org
(package! svg-tag-mode)
(package! org-superstar)
(package! org-bullets)
(package! org-web-tools)
(package! org-protocol-capture-html)
(package! org-pomodoro)
'(package! org-gtd)
(package! org-modern)
(package! org-download)
(package! outshine)

;; lisp

(package! symex
  :recipe (:local-repo "/Users/pierrebaille/Code/forks/symex.el"
           :files ("*.el")))

(package! flycheck-clj-kondo)
(package! janet-mode)

(package! osc)
(package! prettier-js)

(package! bencode
  :recipe (:local-repo "packages"
           :files ("bencode.el")))

(package! treepy)

(package! gptel)
(package! pdf-tools :built-in 'prefer)

(package! mindstream)

(package! reapl-mode
  :recipe (:local-repo "/Users/pierrebaille/Code/WIP/reapl/src/elisp"
           :files ("*.el")))

(package! ligature)

(package! gleam-ts-mode)

(package! clay)

(package! eca :recipe (:host github :repo "editor-code-assistant/eca-emacs" :files ("*.el")))

;; (package! diredfl :disable t)
;; (package! dired+)

(progn :not-used

       '(package! claude-code)

       '(package! centaur-tabs)

       '(package! rigpa
          :recipe (:host github :repo "countvajhula/rigpa"))

       '(package! eaf
          :recipe (:host github
                   :repo "emacs-eaf/emacs-application-framework"
                   :files ("eaf.el" "src/lisp/*.el"))))






;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
                                        ;(package! another-package
                                        ;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
                                        ;(package! this-package
                                        ;  :recipe (:host github :repo "username/repo"
                                        ;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
                                        ;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
                                        ;(package! builtin-package :recipe (:nonrecursive t))
                                        ;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
                                        ;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
                                        ;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
                                        ;(unpin! pinned-package)
;; ...or multiple packages
                                        ;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
                                        ;(unpin! t)
