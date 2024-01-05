(use-package org-roam
    :ensure t
    :init
    (setq
        org-roam-v2-ack t
        ;; Encrypted notes
        org-roam-capture-templates
        '(("d" "default" plain "%?"
              :target (file+head "${slug}.org.gpg" "#+title: ${title}\n"))
             ("w" "Work" entry
                 "* %?"
                 :target (file+head "%<%m-%d-%Y>-work.org.gpg"
                             "#+title: %<%m-%d-%Y> - Work\n")
                 :unarrowed t)
             ("j" "Journal" entry
                 "* %?"
                 :target (file+head "%<%m-%d-%Y>-journal.org.gpg"
                             "#+title: %<%m-%d-%Y> - Journal\n")
                 :unarrowed t))
        ;; Path to daily-notes. The path is relative to
        ;; org-roam-directory
        org-roam-dailies-directory "daily/"
        org-roam-dailies-capture-templates
        '(("d" "default" entry
              "* %?"
              :target (file+head "%<%m-%d-%Y>.org.gpg"
                          "#+title: %<%m-%d-%Y>\n"))))

    :custom
    (org-roam-directory "~/RoamNotes")
    :bind (("C-c n l" . org-roam-buffer-toggle)
              ("C-c n f" . org-roam-node-find)
              ("C-c n i" . org-roam-node-insert)
              ("C-c n d t" . org-roam-dailies-goto-today)
              ("C-c n d g" . org-roam-dailies-goto-date)
              ("C-c n d p" . org-roam-dailies-goto-previous-note)
              ("C-c n d n" . org-roam-dailies-goto-next-note)
              ("C-c n u s" . org-roam-ui-open))
    :config
    (org-roam-setup))

