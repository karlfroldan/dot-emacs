;;; lsp --- Summary
;;; Commentary:
;;; Set up LSP using lsp-mode
;;; Code:

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap
  (setq lsp-keymap-prefix "C-c l")
  :hook ((rust-mode . lsp-deferred)
         (haskell-mode . lsp-deferred)
         (c-mode . lsp-deferred)
         (nix-mode . lsp-deferred)
         (ts-mode . lsp-deferred))
  :config
  (progn
    (lsp-register-client
     (make-lsp-client :new-connection (lsp-tramp-connection "clangd")
                      :major-modes '(c-mode c++-mode)
                      :remote? t
                      :server-id 'clangd-remote)))
  :commands lsp)

(use-package lsp-ui
  :custom
  ((lsp-ui-doc-enable t)
   (lsp-ui-doc-delay 1)
   (lsp-ui-doc-position 'at-point)
   (lsp-ui-doc-show-with-cursor t)
   (lsp-ui-doc-show-with-mouse t)

   ;; Sideline
   (lsp-ui-sideline-show-diagnostics t)
   (lsp-ui-sideline-show-hover t)
   (lsp-ui-sideline-show-code-actions t)
   (lsp-ui-sideline-delay 0.7)))

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
