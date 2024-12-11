(use-package emacs
  :custom
  ((inhibit-startup-screen t)
   ;; No need to remind me what a scratch buffer is
   (initial-scratch-message nil)
   ;; Double spaces after periods is wrong.
   (sentence-end-double-space t)
   ;; Never ding at me, ever
   (ring-bell-function 'ignore)
   ;; Save existing clipboard text into kill ring before replacing
   (save-interprogram-paste-before-kill t)
   ;; Promps should go in the minibuffer, not in a GUI
   (use-dialog-box nil)
   ;; Fix undo in commands affecting the mark
   (mark-even-if-inactive nil)
   ;; Let C-k delete the whole line
   (kill-whole-line t)
   ;; Search should be case sensitive
   (case-fold-search nil)
   ;; No need to prompt for read command _every_ time
   (compilation-read-command nil)
   ;; Scroll to the first error
   (compilation-scroll-output 'first-error)
   ;; Indent using spaces instead of tabs
   (indent-tabs-mode nil)
   ;; Set tab to 4 spaces
   (tab-width 4)

   ;; Allow tramp to use .dir-locals.el
   (enable-remote-dir-locals t)

   ;; LSP MODE Stuff
   ;; Increase gc-threshhold for LSP
   (gc-cons-threshold 100000000)
   ;; Increase the amount of data which emacs reads from the lsp process
   (read-process-output-mmax (* 1024 1024)) ; 1 mb

   ;; Use k&r c
   (c-default-style '((java-mode . "java")
                      (awk-mode . "awk")
                      (other . "k&r")))
   ;; C programming should have 4 spaces
   (c-basic-offset 4)
   ;; Unicode ellipses are better
   (truncate-string-ellipsis "…")
   ;; Backup directory
   (backup-directory-alist '(("." . "~/.bak.emacs"))))

  :config
  ;; Before loading any packages, I want to disable suspending emacs
  (global-unset-key (kbd "C-x C-z"))
  (global-unset-key (kbd "C-z"))

  ;; Enable smooth scrolling
  (pixel-scroll-precision-mode)

  ;; Remove the toolbar
  (tool-bar-mode -1)

  ;; DIRED BEHAVIOR
  (put 'dired-find-alternate-file 'disabled nil)

  (set-charset-priority 'unicode)
  (prefer-coding-system 'utf-8-unix)

  :bind
  (("C->" . scroll-up-line)
   ("C-<" . scroll-down-line))

  :hook
  ((prog-mode . (lambda () (local-set-key (kbd "C-c :") #'goto-line)))
   ;; Set linum-mode for line numbers.
   (prog-mode . display-line-numbers-mode)
   (prog-mode . hl-line-mode)))

;;; Add different binary directories for exec-path
(defun add-to-exec-path (path)
  (let ((path-name (concat (getenv "HOME") "/" path)))
    (when (file-directory-p path-name)
      (message (concat "Adding \'" path-name "\' to exec-path"))
      (add-to-list 'exec-path path-name))))

;;; Allow colors in comint buffers:
;;; https://stackoverflow.com/questions/13397737/ansi-coloring-in-compilation-mode
(defun colorize-compilation-buffer ()
  (ansi-color-apply-on-region compilation-filter-start (point)))

(use-package ansi-color
  :hook ((compilation-filter . colorize-compilation-buffer)))

(provide 'default)
;;; defaults.el ends here
