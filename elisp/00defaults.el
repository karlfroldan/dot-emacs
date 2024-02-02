(setq
 ;; No need to see GNU agitprop
 inhibit-startup-screen nil
 ;; No need to remind me what a scratch buffer is
 initial-scratch-message nil
 ;; Double spaces after periods is wrong
 sentence-end-double-space nil
 ;; Never ding at me, ever.
 ring-bell-function 'ignore
 ;; Save existing clipboard text into kill ring before replacing
 save-interprogram-paste-before-kill t
 ;; prompts should go in the minibuffer, not in a GUI
 use-dialog-box nil
 ;; Fix undo in commands affecting the mark
 mark-even-if-inactive nil
 ;; Let C-k delete the whole line
 kill-whole-line t
 ;; search should be case-sensitive by default
 case-fold-search nil
 ;; No need to prompt for read commend _every_ time
 compilation-read-command nil
 ;; scroll to first error
 compilation-scroll-output 'first-error
 ;; Indent using spaces instead of tabs
 indent-tabs-mode nil
 ;; Set tab as 4 spaces
 tab-width 4
 ;; Unicode ellipses are better
 truncate-string-ellipsis "…"
 ;; Linum-mode line numbers should be relative.
 display-line-numbers-type 'relative)

;; Set linum-mode for line numbers. Note that for newer versions of emacs,
;; we no longer use linum-mode.
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Remove the toolbar and menubars
(scroll-bar-mode -1)
(setq-default indent-tabs-mode nil)

(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8-unix)

(when (display-graphic-p)
  (require 'hl-line)
  (add-hook 'prog-mode-hook #'hl-line-mode)
  (add-hook 'text-mode-hook #'hl-line-mode))

;; Setup smart mode line
(sml/setup)
(setq sml/extra-filler -6)

;; Keybindings for single-line scrolling
(global-set-key (kbd "C-.") 'scroll-up-line)
(global-set-key (kbd "C-,") 'scroll-down-line)

;; Keybinding for switching buffers
(use-package frog-jump-buffer
  :ensure t
  :bind ("C-x C-b" . frog-jump-buffer)
  :config
  (setq frog-jump-buffer-use-all-the-icons-ivy t
        frog-jump-buffer-posframe-parameters
        '((foreground-color . "#e3e3e3")
          (background-color . "#30302e"))))
