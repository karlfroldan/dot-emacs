(setq org-default-notes-file "~/notes/tasks/default.org")

(setq org-capture-templates
      '(("w" "Work-related Task" entry
         (file "~/notes/tasks/work.org")
         "* TODO %?" :empty-lines 1)
        ("j" "Journal" entry
         (file+datetree "~/notes/tasks/journal.org")
         "* %?\nEntered on %U\n %i\n %a")
        ("t" "Personal Task" entry
         (file org-default-notes-file)
         "* TODO %?" :empty-lines 1)))
