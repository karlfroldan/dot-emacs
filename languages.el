(use-package maxima
  :custom ((imaxima-use-maxima-mode-flag nil)
           (maxima-display-maxima-buffer nil))
  :init
  (add-to-list 'auto-mode-alist '("\\.ma[cx]\\'" . maxima-mode))
  (add-to-list 'interpreter-mode-alist
               '("maxima" . 'maxima-mode)))

;; BASIC modes for certain programming modes

(use-package yaml-mode)
(use-package cmake-ts-mode
  :if (treesit-language-available-p 'cmake)
  :defer t)

(use-package python-ts-mode
  :if (treesit-language-available-p 'python)
  :defer t)

(use-package rust-ts-mode
  :if (treesit-language-available-p 'rust)
  :mode "\\.rs\\'"
  :defer t)

(use-package toml-ts-mode
  :if (treesit-language-available-p 'toml)
  :mode "\\.toml\\'"
  :defer t)

(use-package bash-ts-mode
  :if (treesit-language-available-p 'bash)
  :defer t)

(use-package tsx-ts-mode
  :if (treesit-language-available-p 'tsx)
  :custom (tsx-ts-mode-indent-offset 2)
  :mode ("\\.tsx\\'" . tsx-ts-mode)
  :defer t)


(use-package typescript-ts-mode
  :if (treesit-language-available-p 'typescript)
  :custom ((typescript-indent-level 2)
           (typescript-ts-mode-indent-offset 2))
  :mode "\\.ts\\'"
  :defer t)

(use-package c-ts-mode
  :if (treesit-language-available-p 'c)
  :custom
  ((c-ts-mode-indent-offset 4))
  :bind
  :config
  (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c-or-c++-mode . c-or-c++-ts-mode)))

(setq major-mode-remap-alist
      '((c-mode . c-ts-mode)
        (c++-mode . c++-ts-mode)
        (c-or-c++-mode . c-or-c++-ts-mode)
        (rust-mode . rust-ts-mode)
        (cmake-mode . cmake-ts-mode)
        (shell-script-mode . bash-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (toml-mode . toml-ts-mode)))

(defun my/julia-snail-activate-project-root ()
  """Activate a Julia project based on the Projectile root"""
  (interactive)
  (julia-snail-package-activate (projectile-project-root)))

(use-package-with-load-path julia-snail "~/.julia-emacs/julia-snail"
  :custom
  (julia-snail-terminal-type :eat)
  :bind
  (:map julia-snail-mode-map
        ("C-c A" . my/julia-snail-activate-project-root))
  :hook
  (julia-mode . julia-snail-mode))

