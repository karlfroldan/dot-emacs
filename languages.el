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

(use-package rust-ts-mode
  :if (treesit-language-available-p 'rust)
  :mode "\\.rs\\'"
  :defer t)

(use-package toml-ts-mode
  :if (treesit-language-available-p 'toml)
  :mode "\\.toml\\'"
  :defer t)

;; (use-package nix-ts-mode
;;   :if (treesit-language-available-p 'nix)
;;   :defer t)

(use-package haskell-ts-mode
  :if (treesit-language-available-p 'haskell)
  :mode "\\.hs\\'"
  :defer t)

(use-package bash-ts-mode
  :if (treesit-language-available-p 'bash)
  :defer t)

(use-package typescript-ts-mode
  :if (treesit-language-available-p 'typescript)
  :custom ((typescript-indent-level 4)
           (typescript-ts-mode-indent-offset 4))
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

;; Enable ggtags-mode for C mode and C++ mode
(use-package ggtags
  :hook
  ((c-mode-common . (lambda ()
                      (when (derived-mode-p 'c-mode 'c-ts-mode 'c++-mode 'c++-ts-mode 'java-mode)
                        (ggtags-mode 1))))))

(setq major-mode-remap-alist
      '((c-mode . c-ts-mode)
        (c++-mode . c++-ts-mode)
        (c-or-c++-mode . c-or-c++-ts-mode)
        (rust-mode . rust-ts-mode)
        (cmake-mode . cmake-ts-mode)
        (shell-script-mode . bash-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (toml-mode . toml-ts-mode)))
        ;; (nix-mode . nix-ts-mode)))
