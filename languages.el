(use-package haskell-mode
  :config
  (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile))

(use-package maxima
  :custom ((imaxima-use-maxima-mode-flag nil)
           (maxima-display-maxima-buffer nil))
  :init
  (add-to-list 'auto-mode-alist '("\\.ma[cx]\\'" . maxima-mode))
  (add-to-list 'interpreter-mode-alist
               '("maxima" . 'maxima-mode)))

;; BASIC modes for certain programming modes

(use-package yaml-mode)
(use-package cmake-mode)
(use-package rust-mode)

(use-package c-ts-mode
  :if (treesit-language-available-p 'c)
  :custom
  ((c-ts-mode-indent-offset 4))
   ;(c-ts-mode-indent-style "K&R"))
  :config
  (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c-or-c++-mode . c-or-c++-ts-mode)))

;; Tree sitter support
(setq major-mode-remap-alist
      '((c-mode . c-ts-mode)
        (c++-mode . c++-ts-mode)
        (rust-mode . rust-ts-mode)))

;; Enable ggtags-mode for C mode and C++ mode
(use-package ggtags
  :hook
  ((c-mode-common . (lambda ()
                      (when (derived-mode-p 'c-mode 'c-ts-mode 'c++-mode 'c++-ts-mode 'java-mode)
                        (ggtags-mode 1))))))

