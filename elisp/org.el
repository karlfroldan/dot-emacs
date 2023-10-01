(global-set-key (kbd "C-c C-o l") #'org-store-link)
(global-set-key (kbd "C-c C-o a") #'org-agenda)
(global-set-key (kbd "C-c C-o c") #'org-capture)

(require 'org-bullets)

;; Org mode will startup with all unfolded
(setq org-startup-folded nil)
(setq org-level-color-stars-only nil)
(setq org-hide-leading-stars t)

(setq org-bullets-face-name 'org-bullet-face)
(setq org-bullets-byllet-list '(“○” “☉” “◎” “◉” “○” “◌” “◎” “●” “◦” “◯” “⚪” “⚫” “⚬” “❍” “⊙” “⊚” “⊛” “∙” “∘”))

(defun org-mode-hook-cb ()
  (org-bullets-mode 1)
  (org-indent-mode 1))

;; org-mode previews
(add-hook 'org-mode-hook 'org-fragtog-mode)
(add-hook 'org-mode-hook 'org-mode-hook-cb)

;; Setup UTF-8 encoding
(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)

(prefer-coding-system 'utf-8)
(setq default-file-name-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Treat clipboard input as UTF-8 string first
(setq x-select-request-type '(UTF8_STRING COMPOUNT_TEXT TEXT STRING))

;; Set color of code blocks
(require 'color)
(when (display-graphic-p)
  (set-face-attribute 'org-block nil :background
                      (color-darken-name
                       (face-attribute 'default  :background) 3)))
