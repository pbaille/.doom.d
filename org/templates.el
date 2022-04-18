;;; org/templates.el -*- lexical-binding: t; -*-

(setq pb/org-inbox-file
      "~/org/inbox-new.org")

(setq pb/org-capture-simple-properties
      (concat
       ":PROPERTIES:\n"
       ":CREATED: %U\n"
       ":END:\n"
       "\n"))

(setq org-capture-templates

      ` (("p" "Protocol" entry (file ,pb/org-inbox-file)
         ,(concat
           "* [[%:link][%:description]] :link:\n"
           pb/org-capture-simple-properties
           "#+BEGIN_QUOTE\n"
           "%i\n"
           "#+END_QUOTE\n")
         :immediate-finish t
         :empty-lines 1
         :prepend nil)


        ("L" "Protocol Link" entry (file ,pb/org-inbox-file)

         ,(concat
           "* [[%:link][%:description]] :link:\n"
           pb/org-capture-simple-properties)
         :immediate-finish t
         :empty-lines 1
         :prepend nil)


        ("w" "Website" entry (file ,pb/org-inbox-file)

         ,(concat
           "* %c :website:\n"
           pb/org-capture-simple-properties
           "%?\n"
           "\n"
           "%:initial\n")
         :immediate-finish t
         :empty-lines 1
         :prepend t)


        ("k" "Capture" entry (file ,pb/org-inbox-file)

         ,(concat
           "* %(ivy-read \"Title: \" nil :initial-input (if agenda-headlines--prefered-template-key (current-kill 0) \"\")) \n"
           pb/org-capture-simple-properties
           "%i\n"
           "%?")
         :empty-lines 1)


        ("y" "Yankpad" entry (file+function ,yankpad-file aj-org-get-yankpad-target)
         ,(concat
           "** %^{PROMPT} :src: \n"
           pb/org-capture-simple-properties
           "from %a\n"
           "\n"
           "#+BEGIN_SRC %(ivy-read \"Choose language: \" code-capture-src-block-identifiers)\n"
           "%i\n"
           "#+END_SRC\n")
         :immediate-finish t
         :empty-lines 1)


        ("s" "Snippet" entry (file ,pb/org-inbox-file)

         ,(concat
           pb/org-capture-simple-properties
           "from %a\n"
           "\n"
           "#+BEGIN_SRC %(ivy-read \"Choose language: \" code-capture-src-block-identifiers)\n"
           "%i\n"
           "#+END_SRC\n")
         :immediate-finish t
         :empty-lines 1)


        ("t" "Task" entry (file
                           (lambda ()
                             (agenda-filter-funcall-with-filtered-agenda-files #'identity)))
         ,(concat
           "* TO" "DO %(ivy-read \"Title: \" nil :initial-input (if agenda-headlines--prefered-template-key (current-kill 0) \"\")) \n"
           pb/org-capture-simple-properties
           "%i\n"
           "%?"
           "%^{EFFORT}p")
         :empty-lines 1
         :prepend t)


        ("T" "Task clocked-in" entry (file
                                      (lambda ()
                                        (agenda-filter-funcall-with-filtered-agenda-files #'identity)))
         ,(concat
           "* TO" "DO %^{PROMPT} \n"
           pb/org-capture-simple-properties
           "%i\n"
           "%?"
           "%^{EFFORT}p")
         :empty-lines 1
         :clock-in t
         :clock-keep t
         :prepend t)))
