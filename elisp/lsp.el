(use-package lsp-mode
  :init
  ;; Set prefix for lsp-command-keymap
  (setq lsp-keymap-prefix "C-c l")
  :commands lsp)

(add-hook 'c-mode-hook #'lsp)
(add-hook 'c++-mode-hook #'lsp)
(add-hook 'zig-mode-hook #'lsp)
(add-hook 'haskell-mode-hook #'lsp)
(add-hook 'rust-mode-hook #'lsp)
(add-hook 'js2-mode-hook #'lsp)
(add-hook 'rjsx-mode-hook #'lsp)

(use-package company)


