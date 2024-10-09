(setq
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
 ;; Indent using spaces instead of tabs
 indent-tabs-mode nil
 ;; Set tab as 4 spaces
 tab-width 4

 ;;; LSP-mode
 ;; Increase GC Threshold for LSP. It generates a lot of memory (LSP)
 gc-cons-threshold 100000000
 ;; Increase the amount of data whihc emacs reads from the process (LSP)
 read-process-output-max (* 1024 1024) ; 1 mb

 ;; Email address of the current user which is my email address
 user-mail-address "karlfroldan@gmail.com"
 
 ;; C programming style
 c-default-style '((java-mode . "java")
                   (awk-mode . "awk")
                   (other . "k&r"))

 ;; C programming should have 4 spaces
 c-basic-offset 4
 ;; Unicode ellipses are better
 truncate-string-ellipsis "…"
 ;; Backup directory but I think we remove this anyways
 backup-directory-alist '(("." . "~/.bak.emacs")))

 ;; Add to authinfo sources
 ; auth-sources
 ; (list "~/.emacs.d/.authinfo.gpg"))

;; Before loading any packages, I want to disable suspending emacs
(global-unset-key (kbd "C-x C-z"))
(global-unset-key (kbd "C-z"))

;; Single-line scrolling
(global-set-key (kbd "C->") 'scroll-up-line)
(global-set-key (kbd "C-<") 'scroll-down-line)

;; Set linum-mode for line numbers. Note that for newer versions of emacs,
;; we no longer use linum-mode.
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Remove the toolbar and scrollbars because I don't use them anyways.
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(setq-default indent-tabs-mode nil)

(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8-unix)

;;; Add different binary directories for exec-path
(defun add-to-exec-path (path)
  (let ((path-name (concat (getenv "HOME") "/" path)))
    (when (file-directory-p path-name)
      (message (concat "Adding \'" path-name "\' to exec-path"))
      (add-to-list 'exec-path path-name))))

(setq exec-path-list '(".ghcup/bin"
                       ".cabal/bin"
                       ".local/nodejs/bin"
                       ".cargo/bin"))

(mapc #'add-to-exec-path exec-path-list)

;; DIRED BEHAVIOR
(put 'dired-find-alternate-file 'disabled nil)

;;; Allow colors in comint buffers:
;;; https://stackoverflow.com/questions/13397737/ansi-coloring-in-compilation-mode

(provide 'default)
;;; defaults.el ends here
