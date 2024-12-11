(defun relative-org-dir (name)
  "Return a string that's supposed to be a file
   relative to the org-mode notes directory"
  (concat (getenv "HOME") "/Notes/" name))

;; https://stackoverflow.com/questions/17473478/how-to-enter-a-space-in-the-minibuffer-instead-of-completing-a-word
;; I need to use this for org-mode
(define-key minibuffer-local-completion-map "\M- "
            (lambda () (interactive) (insert " ")))

(defun my/org-mode-faces ()
  "Set custom faces for org-mode"
  (let ((my/variable-pitch '(:family "Linux Libertine O" :height 120 :weight thin))
        (my/fixed-pitch '(:family "Fira Code" :height 100)))
    (face-remap-add-relative 'variable-pitch my/variable-pitch)
    (face-remap-add-relative 'fixed-pitch my/fixed-pitch)))

(use-package org
  :hook ((org-mode . turn-on-org-cdlatex)
         (org-mode . variable-pitch-mode)
         (org-mode . visual-line-mode)
         (org-mode . my/org-mode-faces))
  :bind (("C-c o l" . org-store-link)
         ("C-c o a" . org-agenda)
         ("C-c o c" . org-capture)
         ("C-c b x" . org-babel-execute-src-block)
         ("C-c b h" . org-babel-hide-result-toggle))
  
  :custom
  ;; Use RET on keyboard to go to a specified link
  ((org-return-follows-link t)
   ;; I want to see everything unfolded
   (org-startup-folded nil)
   ;; Hide the stars in asterisks
   (org-hide-leading-stars t)
   ;; Use indentation for all org files
   (org-startup-indented t)
   ;; Hide all the emphasis markup like /.../ for italics, *...* for bold, etc.
   (org-hide-emphasis-markers t)
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
                            "* TODO %?" :empty-lines 1))))
  
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((scheme . t)
     (python . t)
     (shell . t)))
  (setq org-babel-python-command "python3")
  (setq org-babel-default-header-args
        '((:session . "my-session")))

  ;; Set the dot character for bullet points
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-+]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  
  
  (let* ((base-font-color (face-foreground 'default nil 'default))
       (headline `(:inherit default :weight bold :foreground ,base-font-color))
       (variable-pitch '(:font "Linux Libertine O" :height 180 :weight thin))
       (fixed-font '(:family "Fira Code" :height 160)))
  (custom-theme-set-faces
   'user
   '(org-block ((t (:inherit fixed-pitch))))
   '(org-block-begin-line ((t (:inherit fixed-pitch :background "#cab9b2" :foreground "#4a0b4a"))))
   '(org-drawer ((t (:inherit fixed-pitch))))
   '(org-code ((t (:inherit (shadow fixed-pitch) :foreground "#2e2d2d" :weight medium))))
   '(org-document-info ((t (:foreground "dark orange"))))
   '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
   '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
   '(org-link ((t (:foreground "royal blue" :underline t))))
   '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-property-value ((t (:inherit fixed-pitch))) t)
   '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
   '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
   '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))

   `(org-level-8 ((t (,@headline ,@variable-pitch))))
   `(org-level-7 ((t (,@headline ,@variable-pitch))))
   `(org-level-6 ((t (,@headline ,@variable-pitch))))
   `(org-level-5 ((t (,@headline ,@variable-pitch))))
   `(org-level-4 ((t (,@headline ,@variable-pitch :height 1.1))))
   `(org-level-3 ((t (,@headline ,@variable-pitch :height 1.125))))
   `(org-level-2 ((t (,@headline ,@variable-pitch :height 1.2))))
   `(org-level-1 ((t (,@headline ,@variable-pitch :height 1.3))))
   `(org-document-title ((t (,@headline ,@variable-pitch :height 1.5 :underline nil)))))))


(use-package cdlatex)

(use-package org-roam
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
                                                     "#+title: ${title}\n#+STARTUP: latexpreview\n")
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
  :config)
