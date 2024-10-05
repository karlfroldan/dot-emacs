;;; lsp --- Summary
;;; Commentary:
;;; Set up LSP using eglot
;;; Code:

;; Automatically enable eglot for these programming languages
(defmacro mode/eglot-ensure (mode)
  """Ensure that eglot will start automatically for the given MODE"""
  `(add-hook (quote ,(intern (concat (symbol-name mode) "-mode-hook"))) 'eglot-ensure))

(defmacro define-keys-for-mode (keymode key-list)
  `(let ((keymap (intern (concat (symbol-name ',keymode) "-map"))))
     (dolist (key-val ,key-list)
       (define-key (symbol-value keymap) (kbd (car key-val)) (cdr key-val)))))

(use-package flymake
  :config
  (define-keys-for-mode flymake-mode
                        '(("C-c f d" . flymake-show-buffer-diagnostics)
                          ("C-c f D" . flymake-show-project-diagnostics))))

(use-package eglot
  :config
  (define-keys-for-mode eglot-mode
                        '(("C-c C-e R" . eglot-reconnect)
                          ("C-c C-e r" . eglot-rename)
                          ("C-c C-e f" . eglot-format)
                          ("C-c C-e c a" . eglot-code-actions)
                          ("C-c C-e c q" . eglot-code-action-quickfix)
                          ("C-c C-e c i" . eglot-code-actions-inline)
                          ("C-c C-e c r" . eglot-code-actions-rewrite)
                          ("C-c C-e c o" . eglot-code-actions-organize-imports)))
  (mode/eglot-ensure haskell))
  ;; For c-mode, we want eglot to start on a per-project basis
  ; (mode/eglot-ensure c)
                                        ; (mode/eglot-ensure c++))

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
