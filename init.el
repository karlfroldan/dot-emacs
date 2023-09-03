(setq inhibit-startup-screen t)
(setq backup-directory-alist '(("." . "~/.bak.emacs")))

(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("nongnu" . "https://elpa.nongnu.org/nongnu")
			 ("melpa" . "https://melpa.org/packages/")))

;; Remove the toolbar
(tool-bar-mode -1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(darcula-theme xcscope scheme-complete all-the-icons-ivy frog-jump-buffer projectile geiser-guile geiser-chicken geiser ghci-completion yaml-mode auctex lsp-haskell company lsp-mode zig-mode vterm-toggle use-package vterm neotree haskell-mode cmake-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; We can install these packages
(mapc #'package-install package-selected-packages)

(package-initialize)

(defun my-load-file (name)
  (load (concat "~/.emacs.d/elisp/" name ".el")))

;; Install vterm later!!!
(setq my-config-files '("haskell" "neotree" "zig" "lsp" "latex" "scheme" "org"))
(mapc 'my-load-file my-config-files)

;; Config file formats
(use-package yaml-mode)

;; frog-jump-buffer
(use-package frog-jump-buffer :ensure t)
(global-set-key (kbd "C-x C-b") 'frog-jump-buffer)
(setq frog-jump-buffer-use-all-the-icons-ivy t)

;; Check if font exists
(require 'cl)
(defun font-candidate (&rest fonts)
  "Return existing font which first match."
  (find-if (lambda (f) (find-font (font-spec :name f))) fonts))

(set-face-attribute 'default nil :font (font-candidate '"Inconsolata-12" "Consolas-12"))
