(use-package maxima
  :ensure t
  :custom ((imaxima-use-maxima-mode-flag nil)
           (maxima-display-maxima-buffer nil))
  :init
  (add-to-list 'load-path "/usr/share/emacs/site-lisp/maxima")
  (add-to-list 'auto-mode-alist '("\\.ma[cx]\\'" . maxima-mode))
  (add-to-list 'interpreter-mode-alist
               '("maxima" . 'maxima-mode))
  (autoload 'imaxima "imaxima" "Image support for Maxima." t))

;; BASIC modes for certain programming modes
(use-package tree-sitter :ensure)
(use-package tree-sitter-langs
  :ensure t
  :config
  (global-tree-sitter-mode))

(use-package yaml-mode
  :ensure t)

(use-package haskell-mode
  :ensure t)

(use-package rust-mode
  :ensure t)

(use-package slint-mode
  :ensure t)

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

(use-package ampl-mode
  :load-path "~/Documents/Projects/ampl-mode/"
  :mode ("\\.mod\\'" . ampl-mode))
