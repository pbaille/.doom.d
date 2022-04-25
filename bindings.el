;;; bindings.el -*- lexical-binding: t; -*- :emacs: :emacs:

(map! :leader
      "o d" #'dired-jump
      "o D" #'+debugger/start
      "d d" #'org-gtd-choose
      "d c" #'org-gtd-capture
      "d e" #'org-gtd-engage
      "d p" #'org-gtd-process-inbox)

(map! :localleader
      (:map org-mode-map
       "c p" #'org-pomodoro))

(map! "s-w" #'ace-window)

(map! (:map symex-editing-mode-map
       "s-e" (lambda () (interactive) (symex-goto-lowest) (symex-evaluate 1))
       "s-i" #'cider-inspect-last-result)

      (:map cider-inspector-mode-map
       :n "j" #'cider-inspector-next-inspectable-object
       :n "k" #'cider-inspector-previous-inspectable-object
       :n "h" #'cider-inspector-pop
       :n "l" #'cider-inspector-operate-on-point)

      (:map ))

'cider-clojuredocs
'cider-doc
'cider-javadoc
