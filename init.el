(setq inhibit-startup-screen t)
(setq backup-directory-alist '(("." . "~/.bak.emacs")))

(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))

;; Remove the toolbar
(tool-bar-mode -1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(auctex lsp-haskell company lsp-mode zig-mode vterm-toggle use-package vterm neotree haskell-mode cmake-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; We can install these packages
(mapc #'package-install package-selected-packages)

(defun my-load-file (name)
  (load (concat "~/.emacs.d/elisp/" name ".el")))

;; Install vterm later!!!
(setq my-config-files '("haskell" "neotree" "zig" "lsp" "latex" "scheme"))
(mapc 'my-load-file my-config-files)
