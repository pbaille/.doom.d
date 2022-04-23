;;; bindings.el -*- lexical-binding: t; -*-

(map! :leader
      "o d" #'dired-jump
      "o D" #'+debugger/start
      )

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
       :n "l" #'cider-inspector-operate-on-point))

'cider-clojuredocs
'cider-doc
'cider-javadoc
