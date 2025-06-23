(defvar my/base-org-dir (concat (getenv "HOME") "/Documents/Notes"))

(defun org-dir (name)
  "Return a string that's supposed to be the file NAME
   relative to the org-mode notes directory."
  (concat my/base-org-dir "/" name))

;; https://stackoverflow.com/questions/17473478/how-to-enter-a-space-in-the-minibuffer-instead-of-completing-a-word
;; I need to use this for org-mode
;; Insert space while in the mini-buffer
(define-key minibuffer-local-completion-map "\M- "
            (lambda () (interactive (insert " "))))

(use-package cdlatex)

(use-package org
  :hook ((org-mode . turn-on-org-cdlatex)
         ;; (org-mode . variable-pitch-mode)
         (org-mode . visual-line-mode))
  :bind (("C-c o l" . org-store-link)
         ("C-c o a" . org-agenda)
         ("C-c o c" . org-capture)
         ("C-c b x" . org-babel-execute-src-block)
         ("C-c b h" . org-babel-hide-result-toggle))
  :custom
  ;; Use RET on keyboard to go to a specific link
  ((org-return-follows-link t)
   ;; I want to see everything unfolded
   (org-startup-folded nil)
   ;; Hide the stars in the asterisks
   (org-hide-leading-stars t)
   ;; Use indentation for all org-files
   (org-startup-indented t)
   ;; Hide all the emphasis markup like /.../ for italics, *...* for bold, etc.
   (org-hide-emphasis-markers t)
   ;; Default note types for org-mode
   (org-default-notes-file (org-dir "tasks/default.org")))

  :config
  (ensure-directory-exists my/base-org-dir)

  (org-babel-do-load-languages
   'org-babel-do-load-languages
   '((scheme . t)
     (python . t)
     (julia . t)
     (shell . t)))
  (setq org-babel-python-command "python3")
  (setq org-babel-default-header-args '((:session . "my-session"))))

(use-package org-roam
  :custom ((org-roam-directory (org-dir "roam"))
           (org-roam-v2-ack t)
           (org-roam-dailies-directory "daily/")
           (org-roam-dailies-capture-templates
            '(("d" "default" entry
               "* %<%H:%M> %?"
               :if-new (file+head "%<%Y-%m-%d>.org"
                                  "#+title: %<%Y-%m-%d>\n#+filetags: :daily:")
               :unarrowed t)
              ("s" "Study Session" entry
               "* Study: %^{Topic}\n:PROPERTIES:\n:Time: %U\n:END:\n\nResources:\n- \n\nNotes:\n%?"
               :if-new (file+head+olp "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n#+filetags: :daily:\n#+STARTUP: latexpreview" ("Study Sessions")))
              ("r" "Reading Session" entry
               "* Reading: %^{Title}\n:PROPERTIES:\n:Source: %^{Source}\n:Time: %U\n:END:\n\nWhat I learned:\n%?"
               :if-new (file+head+olp "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n#+STARTUP: latexpreview" ("Readings")))
              ("w" "Work Log" entry
               "* %U - %^{Task} \n%?"
               :if-new (file+head+olp "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n#+STARTUP: latexpreview" ("Research")))

              ("i" "Idea" entry
               "* Idea: %^{Title}\n%?\n\nRelated: [[id:]]"
               :if-new (file+head+olp "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n#+STARTUP: latexpreview" ("Ideas")))))
           (org-roam-capture-templates
            '(("l" "Literature Note" plain
               "* Title: ${title}\n + Authors: ${authors}\n + Source: ${source}\n + Year: ${year}\n + Keywords: ${keywords}\n\n* Summary\n${summary}\n\n* Key Points\n +\n\n* My Thoughts\n +\n\n* Related Papers\n +"
               :target (file+head "literature/${slug}.org" "#+title: ${title}\n#+STARTUP: latexpreview")
               :unarrowed t)

              ("r" "Research Log" plain
               "* Date: %U\n* Focus Area: ${area}\n* What I did today\n +\n\n* Problems\n +\n\n* Next Steps\n + \n\n* Notes\n +\n\n* References\n +"
               :target (file+datetree "research/research-log.org")
               :unarrowed t)

              ("d" "Definition / Concept" plain
               "* Definition\n${definition}\n\n* Examples\n +\n\n* Related Concepts\n +"
               :target (file+head "concepts/${slug}.org" "#+title: ${title}\n#+STARTUP: latexpreview")
               :unarrowed t))))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n t" . org-roam-tag-add)
         ("C-c n d c" . org-roam-dailies-capture-today)
         ("C-c n d t" . org-roam-dailies-goto-today)
         ("C-c n d g" . org-roam-dailies-goto-date)
         ("C-c n d p" . org-roam-dailies-goto-previous-note)
         ("C-c n d n" . org-roam-dailies-goto-next-note)
         ("C-c n u s" . org-roam-ui-open))
  :config
  (org-roam-setup))

(use-package org-roam-bibtex
  :after org-roam)

           
           
