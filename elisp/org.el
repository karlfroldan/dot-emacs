(global-set-key (kbd "C-c C-o l") #'org-store-link)
(global-set-key (kbd "C-c C-o a") #'org-agenda)
(global-set-key (kbd "C-c C-o c") #'org-capture)

;; org-mode previews
(add-hook 'org-mode-hook 'org-fragtog-mode)
