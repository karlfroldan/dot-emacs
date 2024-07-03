;;; lsp --- Summary
;;; Commentary:
;;; Set up LSP using eglot
;;; Code:

;; Automatically enable eglot for these programming languages
(defmacro mode/eglot-ensure (mode)
  """Ensure that eglot will start automatically for the given MODE"""
  `(add-hook (quote ,(intern (concat (symbol-name mode) "-mode-hook"))) 'eglot-ensure))

(use-package eglot
  :config
  (mode/eglot-ensure haskell)
  (mode/eglot-ensure c)
  (mode/eglot-ensure c++))

(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(provide 'lsp)
;;; lsp.el ends here
