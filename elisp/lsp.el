(use-package lsp-mode
  :init
  ;; Set prefix for lsp-command-keymap
  (setq lsp-keymap-prefix "C-c l")
  :commands lsp)

(add-hook 'zig-mode-hook #'lsp)
(add-hook 'haskell-mode'hook #'lsp)

(use-package company)


