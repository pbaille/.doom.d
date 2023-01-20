;;; org/templates.el -*- lexical-binding: t; -*-

(setq pb/org-inbox-file
      "~/org/gtd/inbox.org")

(setq pb/org-capture-simple-properties
      (concat
       ":PROPERTIES:\n"
       ":CREATED: %U\n"
       ":END:\n"
       "\n"))


(setq org-capture-templates

      `(("t" "Personal todo" entry
         (file+headline +org-capture-todo-file "Inbox")
         "* [ ] %?\n%i\n%a" :prepend t)

        ("n" "Personal notes" entry
         (file+headline +org-capture-notes-file "Inbox")
         "* %u %?\n%i\n%a" :prepend t)

        ("j" "Journal" entry
         (file+olp+datetree +org-capture-journal-file)
         "* %U %?\n%i\n%a" :prepend t)

        ("l" "links")

        ("ll" "link" entry (file ,pb/org-inbox-file)
         ,(concat
           "* [[%:link][%:description]] :link:\n"
           pb/org-capture-simple-properties
           "#+BEGIN_QUOTE\n"
           "%i\n"
           "#+END_QUOTE\n")
         :immediate-finish t
         :empty-lines 1
         :prepend nil)

        ("lt" "link tagged" entry (file ,pb/org-inbox-file)
         ,(concat
           "* [[%:link][%:description]] %^g\n"
           pb/org-capture-simple-properties
           "%?\n")
         :empty-lines 1
         :prepend nil)


        ("ls" "short link" entry (file ,pb/org-inbox-file)
         ,(concat
           "* [[%:link][%:description]] :link:\n"
           pb/org-capture-simple-properties)
         :immediate-finish t
         :empty-lines 1
         :prepend nil)



        ("p" "Templates for projects")

        ("pt" "Project-local todo" entry
         (file+headline +org-capture-project-todo-file "Inbox")
         "* TODO %?\n%i\n%a" :prepend t)

        ("pn" "Project-local notes" entry
         (file+headline +org-capture-project-notes-file "Inbox")
         "* %U %?\n%i\n%a" :prepend t)

        ("pc" "Project-local changelog" entry
         (file+headline +org-capture-project-changelog-file "Unreleased")
         "* %U %?\n%i\n%a" :prepend t)



        ("o" "Centralized templates for projects")

        ("ot" "Project todo" entry
         #'+org-capture-central-project-todo-file "* TODO %?\n %i\n %a"
         :heading "Tasks" :prepend nil)

        ("on" "Project notes" entry
         #'+org-capture-central-project-notes-file "* %U %?\n %i\n %a"
         :heading "Notes" :prepend t)

        ("oc" "Project changelog" entry
         #'+org-capture-central-project-changelog-file "* %U %?\n %i\n %a"
         :heading "Changelog" :prepend t)

        ("w" "Web site" entry
         (file "")
         "* %a :website:\n\n%U %?\n\n%:initial")))





















'(("w" "Website" entry (file ,pb/org-inbox-file)

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
   :prepend t))
