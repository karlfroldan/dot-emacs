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
