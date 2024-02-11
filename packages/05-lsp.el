(use-package lsp-mode
  :commands lsp
  :init
  ;; Set prefix for lsp-command-keymap
  (setq lsp-keymap-prefix "C-c l")
  :config
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  :hook (lsp-mode . lsp-enable-which-key-integration))

(defmacro mapc-load-lsp (modes)
  `(mapc (lambda (mode)
           (add-hook (intern (concat (symbol-name mode) "-mode-hook")) #'lsp))
         ',modes))

(mapc-load-lsp (c c++ haskell rust shell-script))

(use-package company)


