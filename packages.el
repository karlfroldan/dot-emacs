;; ---- SOME UTILITY FUNCTIONS FOR LOADING PACKAGES ----
(defun font-candidate (&rest fonts)
  "Return existing font which first matches."
  (cl-find-if (lambda (f)
                (find-font (font-spec :name f))) fonts))

(defmacro mapc-load-lsp (modes)
  `(mapc (lambda (mode)
           (add-hook (intern (concat (symbol-name mode) "-mode-hook")) #'lsp))
         ',modes))

(setq karl/emacs-theme 'material)

;; ---- LOADING PACKAGES START ----

;; Compatibility library for emacs < 24.3
(use-package cl-lib
  :ensure t)

(use-package all-the-icons
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :after all-the-icons
  :if (display-graphic-p)
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

;; smart-mode line
(use-package smart-mode-line
  :ensure t
  :config
  (sml/setup)
  (setq sml/extra-filler -6))

;; Frog-jump buffer will let us jump between multiple
;; buffers flawlessly using C-x C-b
(use-package frog-jump-buffer
  :ensure t
  :bind ("C-x C-b" . frog-jump-buffer)
  :config
  (setq
   ;; Enable icons in our frog-jump buffer
   frog-jump-buffer-use-all-the-icons-ivy t
   frog-jump-buffer-posframe-parameters '((foreground-color . "#e3e3e3")
                                          (background-color . "#30302e"))))

;; Load theme
(use-package material-theme
  :ensure t
  :if (display-graphic-p)
  :config
  (set-face-attribute 'default nil
                      :font
                      (font-candidate
                       '"Fira Code:size=14"
                       "Inconsolata-12"
                       "Consolas-12"))
  (load-theme 'material))

;; Quick browsing, filtering, searching, and indexing of plain text files.
;; We use this for our own org-mode notes.
(use-package deft
  :ensure t
  :after org
  :bind
  ("C-c n s" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory org-roam-directory))

;; ERC is a basic IRC chat client.
(use-package erc-services
  :config
  (erc-services-mode 1)
  (setq
   ;; We use the stored password. This password is encrypted.
   erc-prompt-for-nickserv-password nil
   erc-nick "fireking04"
   erc-user-full-name "fireking04")
  (erc-services-mode 1)
  (defun libera-chat-serv ()
    (interactive)
    (erc-tls :server "irc.libera.chat"
             :port "6697")))

;; Haskell programminglanguage
(use-package haskell-mode)
(use-package lsp-haskell
  :after haskell-mode)

;; LaTeX typesetting
(use-package tex
  :ensure auctex
  :config
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode))

;; Scheme programming language
;; TODO: Active scheme-mode when opening a scheme file.
(use-package geiser
  :config
  (setq geiser-active-implementations '(chicken))
  (add-hook 'scheme-mode-hook 'geiser-mode))

(use-package lsp-mode
  :commands lsp
  :ensure t
  :init
  ;; All LSP related commands start with C-c l
  (setq lsp-keymap-prefix "C-c l")
  :config
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)

  ;; Load LSP for the following programming languages.
  (mapc-load-lsp (c c++ haskell rust shell-script))
  
  :hook
  (lsp-mode . lsp-enable-which-key-integration))

;; Text-completion framework that can integrate well with lsp-mode
(use-package company
  :ensure t)

;; Emacs git client
(use-package magit
  :ensure t)

;; Completion suggestions in the minibuffer
(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  (which-key-setup-minibuffer))

;; Ripgrep is a search tool like grep written in Rust
(use-package rg
  :ensure-system-package rg
  :config
  (rg-enable-default-bindings))

;; Projectile is a project interaction library for emacs. Basically,
;; this makes emacs like eclipse or netbeans where we can open projects
;; instead of individual files (like vim)
(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))
