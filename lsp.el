;;; lsp --- Summary
;;; Commentary:
;;; Set up LSP using eglot
;;; Code:

;; Automatically enable eglot for these programming languages
(defvar my/eglot-mode-list
  '(haskell-mode
    c-mode
    c++-mode))

(use-package eglot
  :config
  (add-hook 'haskell-mode-hook 'eglot-ensure))

(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(provide 'lsp)
;;; lsp.el ends here
