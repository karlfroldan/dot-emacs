(use-package haskell-mode
  :ensure t
  :config
  (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile))

(use-package maxima
  :ensure t
  :custom ((imaxima-use-maxima-mode-flag nil)
           (maxima-display-maxima-buffer nil))
  :init
  (add-to-list 'auto-mode-alist '("\\.ma[cx]\\'" . maxima-mode))
  (add-to-list 'interpreter-mode-alist
               '("maxima" . 'maxima-mode)))

;; BASIC modes for certain programming modes
(use-package yaml-mode :ensure t)
(use-package yang-mode :ensure t)
(use-package cmake-mode :ensure t)
(use-package rust-mode :ensure t)

;; Tree sitter support
(setq major-mode-remap-alist
      '((c-mode . c-ts-mode)
        ;(c++-mode . c++-ts-mode)
        (rust-mode . rust-ts-mode)))

