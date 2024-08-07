;;; PACKAGES --- SOME UTILITY FUNCTIONS FOR LOADING PACKAGES ---

(defun font-candidate (&rest fonts)
  "Return existing FONTS which first matches."
  (cl-find-if (lambda (f)
                (find-font (font-spec :name f))) fonts))

(defmacro mapc-load-lsp (modes)
  `(mapc (lambda (mode)
           (add-hook (intern (concat (symbol-name mode) "-mode-hook")) #'lsp))
         ',modes))

;; ---- LOADING PACKAGES START ----

;; Used for magit keyboard commands.
(use-package transient :ensure t)

(use-package all-the-icons
  :ensure t)

(use-package all-the-icons-dired
  :ensure t
  :after all-the-icons
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

;; smart-mode line
(use-package smart-mode-line-atom-one-dark-theme
  :ensure t)

(use-package smart-mode-line
  :ensure t
  :config
  (setq sml/extra-filler -6
        sml/theme 'atom-one-dark)
  (sml/setup))

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
                                          (background-color . "#30302e")))
  ;; Ignore some buffers that I'm not interested in. For reference, instead
  ;; of C-x C-b, I can open these buffers using C-x b.
  (dolist (regexp '("TAGS" "^\\*Compile-log" "-debug\\*$"
                    "errors\\*$" "^\\*Backtrace" "-ls\\*$"
                    "stderr\\*$" "^\\*Flymake" "^\\*vc"
                    "^\\*Warnings" "^\\*eldoc" "\\^*Shell Command"
                    "\\*lsp-log\\*" "\\*Completions\\*"
                    "-compile-Log\\*$" "\\*clangd\\*"))
    (push regexp frog-jump-buffer-ignore-buffers)))

;; Load theme
(use-package material-theme
  :ensure t
  :config
  (my/load-make-after-frame
   (set-face-attribute 'default nil
                       :font
                       (font-candidate
                        '"Fira Code:size=14"))
   (load-theme 'material)))


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
  :ensure t
  ;; :ensure-system-package rg
  :config
  (rg-enable-default-bindings))

;; Projectile is a project interaction library for emacs. Basically,
;; this makes emacs like eclipse or netbeans where we can open projects
;; instead of individual files (like vim)
(use-package projectile
  :ensure t
  :init
  (setq projectile-keymap-prefix (kbd "C-c C-p"))
  :config
  (setq projectile-project-search-path '("~/projects/"
                                         "~/sources/"))
  (projectile-global-mode))

(use-package haskell-mode
  :ensure t
  :config
  (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile))
  

(use-package maxima
  :ensure t
  :init
  (setq imaxima-use-maxima-mode-flag nil
        maxima-display-maxima-buffer nil)
  (add-to-list 'auto-mode-alist '("\\.ma[cx]\\'" . maxima-mode))
  (add-to-list 'interpreter-mode-alist
               '("maxima" . 'maxima-mode)))

(use-package yasnippet
  :ensure t
  :config
  (setq yas-snippet-dirs
        '("~/.emacs.d/snippets"))
  (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode))

;;; EAT Terminal Emulator
(use-package eat
  :ensure t
  :config
  (add-hook 'eshell-load-hook #'eat-eshell-mode)
  (add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode))

(use-package counsel
  :ensure t)

(use-package tramp
  :config
  (add-to-list 'tramp-remote-path "~/.local/bin"))

;; BASIC modes for certain programming modes
(use-package yaml-mode :ensure t)
(use-package yang-mode :ensure t)
(use-package cmake-mode :ensure t)
(use-package rust-mode :ensure t)

(provide 'packages)
;;; packages.el ends here

