(setq org-default-notes-file (relative-org-dir "tasks/default.org"))

(setq org-capture-templates
      '(("w" "Work-related Task" entry
         (file (relative-org-dir "tasks/work.org"))
         "* TODO %?" :empty-lines 1)
        ("j" "Journal" entry
         (file+datetree (relative-org-dir "tasks/journal.org"))
         "* %?\nEntered on %U\n %i\n %a")
        ("t" "Personal Task" entry
         (file org-default-notes-file)
         "* TODO %?" :empty-lines 1)))
