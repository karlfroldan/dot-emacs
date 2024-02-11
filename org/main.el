(defun load-org-elisp (file-name)
  (load-elisp-file (concat "org/" file-name)))

(defun relative-org-dir (name)
  "Return a string that's supposed to be a
   file relative to the org-mode notes directory"
  (concat (getenv "HOME") "/notes/" name))

;; General org-mode settings
(setq
 ;; Use RET on keyboard to go to the specified link
 org-return-follows-link t
 ;; I want to see everything
 org-startup-folded nil
 ;; Use indentation for all org files
 org-startup-indented t)

(add-hook 'org-mode-hook '(lambda () (setq fill-column 80)))
(add-hook 'org-mode-hook 'auto-fill-mode)

(setq org-mode-elisp-files '("keybindings.el" "roam.el" "tasks.el"))

(mapc #'load-org-elisp org-mode-elisp-files)
