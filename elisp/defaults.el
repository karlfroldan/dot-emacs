(setq
 ;; No need to see GNU agitprop
 inhibit-startup-screen t
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
 ;; Unicode ellipses are better
 truncate-string-ellipsis "…")
;; Remove the toolbar and menubars
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq-default indent-tabs-mode nil)

(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8-unix)

(require 'hl-line)
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'text-mode-hook #'hl-line-mode)

;;(setq
;; make-backup-files nil
;; auto-save-default nil
;; create-lockfiles nil)

