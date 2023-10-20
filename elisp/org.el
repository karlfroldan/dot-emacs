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

;; Set org mode \d to be the differential
(defun expand-d-for-differential (var)
  (format "\\frac{\\text{d}}{\\text{d}%s}" var))

(add-hook 'org-mode-hook (lambda ()
          (define-abbrev org-mode-abbrev-table "diff"
            'expand-d-for-differential)))

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

(defun org-mode-variadic-headers ()
  (custom-set-faces
   '(org-level-1 ((t (:inherit outline-1 :height 1.6))))
   '(org-level-2 ((t (:inherit outline-2 :height 1.4))))
   '(org-level-3 ((t (:inherit outline-3 :height 1.2))))
   '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
   '(org-level-5 ((t (:inherit outline-5 :height 1.0))))))

;; Set color of code blocks
(require 'color)
(when (display-graphic-p)
  (setq org-export-with-tex t)
  (setq org-export-with-verbatim t)
  (set-face-attribute 'org-block nil :background
                      (color-darken-name
                       (face-attribute 'default  :background) 3))
  (org-mode-variadic-headers)
  (setq org-mode-format-latex-options
        (plist-put org-format-latex-options
                   :scale 1.5)))

;; Babel stuff
(org-babel-do-load-languages
 'org-babel-load-languages
 '((haskell . t)))
