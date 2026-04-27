;; BASIC modes for certain programming modes
(use-package tree-sitter
  :ensure t
  :hook ((rust-mode . tree-sitter-mode)
         (c-mode . tree-sitter-mode)
         (c++-mode . tree-sitter-mode)
         (python-mode . tree-sitter-mode)))

(use-package tree-sitter-langs
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package haskell-mode
  :ensure t)

(use-package rust-mode
  :ensure t)

(use-package dockerfile-mode
  :ensure t)

(use-package uv-mode
  :ensure t
  :hook (python-mode . uv-mode-auto-activate-hook))

(use-package julia-mode
  :ensure t
  :custom ((julia-latexsub-greedy . nil)))

(defun my/julia-snail-start ()
  "Start Julia Snail in a non-active window with Projectile root."
  (interactive)
  (let ((root (projectile-project-root)))
    (unless root
      (error "Not in a Projectile project"))
    (let ((default-directory root))
      (if (one-window-p)
          (split-window-sensibly)
        (other-window 1))
      (julia-snail))))


(defun my/julia-snail-activate-project-root ()
  "Activate a Julia project based on the Projectile root"
  (interactive)
  (julia-snail-package-activate (projectile-project-root)))

(use-package julia-snail
  :ensure t
  :custom ((julia-snail-terminal-type :eat))
           ;; (julia-snail-executable "~/.juliaup/bin/julia"))
  :bind
  (:map julia-snail-mode-map
        ("C-c A" . my/julia-snail-activate-project-root)
        ("C-c C-s" . my/julia-snail-start))
  :hook
  (julia-mode . julia-snail-mode))
