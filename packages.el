;;; PACKAGES --- SOME UTILITY FUNCTIONS FOR LOADING PACKAGES ---

(defmacro mapc-load-lsp (modes)
  `(mapc (lambda (mode)
           (add-hook (intern (concat (symbol-name mode) "-mode-hook")) #'lsp))
         ',modes))

;; ---- LOADING PACKAGES START ----

;; Used for magit keyboard commands.
(use-package transient :ensure t)

(use-package all-the-icons)

(use-package all-the-icons-dired
  :after all-the-icons
  :custom ((all-the-icons-dired-monochrome nil))
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package smart-mode-line
  :custom ((sml/extra-filler -6)
           (sml/mode-width 'full))
  :config
  (sml/setup))

;; Frog-jump buffer will let us jump between multiple
;; buffers flawlessly using C-x C-b
(use-package frog-jump-buffer
  :bind ("C-x C-b" . frog-jump-buffer)
  :config
  (setq-default
   frog-jump-buffer-use-all-the-icons-ivy t
   frog-jump-buffer-posframe-parameters '((foreground-color . "#f1f1f1")
                                          (background-color . "#2a2a2a")))
  ;; Ignore some buffers that I'm not interested in. For reference, instead
  ;; of C-x C-b, I can open these buffers using C-x b.
  (dolist (regexp '("TAGS" "^\\*Compile-log" "-debug\\*$"
                    "errors\\*$" "^\\*Backtrace" "-ls\\*$"
                    "stderr\\*$" "^\\*Flymake" "^\\*vc"
                    "^\\*Warnings" "^\\*eldoc" "\\^*Shell Command"
                    "\\*lsp-log\\*" "\\*Completions\\*"
                    "-compile-Log\\*$" "\\*clangd\\*"))
    (push regexp frog-jump-buffer-ignore-buffers)))

(use-package auctex)

;; Quick browsing, filtering, searching, and indexing of plain text files.
;; We use this for our own org-mode notes.
(use-package deft
  :after org
  :bind
  ("C-c n s" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory org-roam-directory))

;; Emacs git client
(use-package magit)

;; Completion suggestions in the minibuffer
(use-package which-key
  :config
  (which-key-mode)
  (which-key-setup-minibuffer))

;; Ripgrep is a search tool like grep written in Rust
(use-package rg
  ;; :ensure-system-package rg
  :config
  (rg-enable-default-bindings))

;; Projectile is a project interaction library for emacs. Basically,
;; this makes emacs like eclipse or netbeans where we can open projects
;; instead of individual files (like vim)
(use-package projectile
  :init
  (setq projectile-keymap-prefix (kbd "C-c p")
        projectile-project-search-path '("~/Documents/Projects/"))
  :config
  (projectile-mode +1))

(use-package yasnippet
  :custom (yas-snippet-dirs '("~/.emacs.d/snippets"))
  :hook ((prog-mode . yas-minor-mode)
         (org-mode . yas-minor-mode))
  :config
  (yas-reload-all))

;;; EAT Terminal Emulator
(use-package eat
  :hook ((eshell-load . eat-eshell-mode)
         (eshell-load . eat-eshell-visual-command-mode))
  :config
  (define-key eat-semi-char-mode-map (kbd "M-o") nil))

(use-package tramp
  :config
  (add-to-list 'tramp-remote-path "~/.local/bin")
  ;; Haskell stuff
  (add-to-list 'tramp-remote-path "~/.cabal/bin")
  (add-to-list 'tramp-remote-path "~/.ghcup/bin"))

(use-package avy
  :custom
  ;; Set the avy timeout for avy-goto-char-timer to 0.8 seconds
  (avy-timeout-seconds 0.8)
  :bind
  ;; Input 1 char, jump to it with a tree.
  (("C-c a 1" . avy-goto-char)
   ;; Input 2 consecutive chars, jump to the first one with a tree.
   ("C-c a 2" . avy-goto-char-2)
   ;; Input multiple chars and jump to the first one after some
   ;; amount of time.
   ("C-c a m" . avy-goto-char-timer)
   ;; Input zero chars, jump to a line start with a tree
   ("C-c a 0" . avy-goto-line)
   ;; Input one char at a character start, jump to a word start with a tree
   ("C-c a w" . avy-goto-word-1))
  :config
  (avy-setup-default))

(use-package rich-minority
  :config
  (add-to-list 'rm-blacklist " yas")
  (add-to-list 'rm-blacklist " company")
  (add-to-list 'rm-blacklist " all-the-icons-dired-mode"))

(use-package buffer-env
  :hook ((comint-mode . hack-dir-local-variables-non-file-buffer)
         (hack-local-variables . buffer-env-update))
  :custom ((buffer-env-script-name '("flake.nix"))
           (buffer-env-verbose t)))

(use-package annotate)

(use-package ace-window
  :bind ("M-o" . ace-window))

(use-package windresize
  :bind ("C-c C-w" . windresize))

(use-package age
  :demand t
  :config
  (age-file-enable)
  (add-to-list 'auth-sources "~/.authinfo.age")
  :custom ((age-default-identity "~/.ssh/id_ed25519")
           (age-default-recipient "~/.ssh/id_ed25519.pub")))

(provide 'packages)
;;; packages.el ends here
