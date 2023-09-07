(require 'haskell-mode)
(require 'lsp-haskell)

;; Haskell keybindings.
;; All haskell keybindings will begin with the prefix
;; C-c C-k
(define-key haskell-mode-map (kbd "C-c C-k i") 'haskell-navigate-imports)
(define-key haskell-mode-map (kbd "C-c C-k c") 'haskell-compile)
