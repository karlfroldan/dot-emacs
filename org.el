(defun relative-org-dir (name)
  "Return a string that's supposed to be a file
   relative to the org-mode notes directory"
  (concat (getenv "HOME") "/Notes/" name))

;; https://stackoverflow.com/questions/17473478/how-to-enter-a-space-in-the-minibuffer-instead-of-completing-a-word
;; I need to use this for org-mode
(define-key minibuffer-local-completion-map "\M- "
            (lambda () (interactive) (insert " ")))

(use-package org
  :ensure t
  :hook ((org-mode . turn-on-org-cdlatex)
         (org-mode . visual-line-mode))
  :bind (("C-c o l" . org-store-link)
         ("C-c o a" . org-agenda)
         ("C-c o c" . org-capture))
  :custom
  ;; Use RET on keyboard to go to a specified link
  ((org-return-follows-link t)
   ;; I want to see everything unfolded
   (org-startup-folded nil)
   ;; Use indentation for all org files
   (org-startup-indented t)
   ;; Default note types for org-mode
   (org-defaults-notes-file (relative-org-dir "tasks/default.org"))
   (org-capture-templates '(("w" "Work-related Task" entry
                            (file (relative-org-dir "tasks/work.org"))
                            "* TODO %?" :empty-lines 1)
                           ("j" "Journal" entry
                            (file+datetree (relative-org-dir "tasks/journal.org"))
                            "* %?\nEntered on %U\n %i\n %a")
                           ("t" "Personal Task" entry
                            (file org-default-notes-file)
                            "* TODO %?" :empty-lines 1)))))

(use-package cdlatex
  :ensure auctex)

(use-package org-roam
  :ensure t
  :after (cdlatex org)
  :custom
  ((org-roam-directory (relative-org-dir "roam"))
   (org-roam-v2-ack t)
   (org-roam-dailies-directory "daily/")
   (org-roam-dailies-capture-templates '(("d" "default" entry
                                          "* %?"
                                          :target (file+head "%<%Y-%m-%d>.org"
                                                             "#+title: %<%m-%d-%Y>\n"))))
   (org-roam-capture-templates '(("d" "main" plain "%?"
                                  :if-new (file+head "main/${slug}.org"
                                                     "#+title: ${title}\n")
                                  :unarrowed t)
                                 ("r" "reference" plain "%?"
                                  :if-new (file+head "reference/${slug}.org"
                                                     "#+title: ${title}\n"))
                                 ("l" "literature" plain "%?"
                                  :if-new (file+head "literature/${slug}.org"
                                                     "#+title: ${title}\n")))))
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
  :after org-roam
  :ensure t
  :config)
