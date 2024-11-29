;;; lsp --- Summary
;;; Commentary:
;;; Set up LSP using lsp-mode
;;; Code:

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap
  (setq lsp-keymap-prefix "C-c l")
  :hook ((rust-mode . lsp)
         (haskell-mode . lsp))
         ;; (nix-mode . lsp))
  :commands lsp)

;; lsp-mode integration for emacs
(use-package lsp-nix
  :after (lsp-mode)
  :custom
  (lsp-nix-nil-formatter ["nixfmt"]))

(use-package ggtags
  :hook (c-mode . ggtags-mode)
  :bind (("C-c g d" . ggtags-find-definition)
         ("C-c g r" . ggtags-find-references)
         ("C-c g R" . ggtags-query-replace)))

(use-package company
  :custom ((company-tooltip-align-annotations t)
           (company-tooltip-limit 6)
           (company-tooltip-maximum-width 60)
           ;; Candidate icons will show a letter
           ;; corresponding to the candidate type.
           (company-format-margin-function 'company-text-icons-margin)
           (company-text-icons-add-background t))
  :config (global-company-mode))

(provide 'lsp)
;;; lsp.el ends here
