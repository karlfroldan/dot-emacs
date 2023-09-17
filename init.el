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
 '(custom-safe-themes
   '("4b026ac68a1aa4d1a91879b64f54c2490b4ecad8b64de5b1865bca0addd053d9" "90a6f96a4665a6a56e36dec873a15cbedf761c51ec08dd993d6604e32dd45940" "f149d9986497e8877e0bd1981d1bef8c8a6d35be7d82cba193ad7e46f0989f6a" default))
 '(ispell-dictionary nil)
 '(package-selected-packages
   '(flucui-themes julia-mode magit modus-themes material-theme darcula-theme xcscope scheme-complete all-the-icons-ivy frog-jump-buffer projectile geiser-guile geiser-chicken geiser ghci-completion yaml-mode auctex lsp-haskell company lsp-mode zig-mode vterm-toggle use-package vterm haskell-mode cmake-mode)))
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
(setq my-config-files '("haskell" "zig" "lsp" "latex" "scheme" "org" "julia"))
(mapc 'my-load-file my-config-files)

;; Config file formats
(use-package yaml-mode)

;; frog-jump-buffer
(use-package frog-jump-buffer :ensure t)
(global-set-key (kbd "C-x C-b") 'frog-jump-buffer)
(setq frog-jump-buffer-use-all-the-icons-ivy t)

;; Check if font exists
(require 'cl-lib)
(defun font-candidate (&rest fonts)
  "Return existing font which first match."
  (cl-find-if (lambda (f) (find-font (font-spec :name f))) fonts))

(set-face-attribute 'default nil :font
		    (font-candidate
		     '"Source Code Pro:size=16"
		     "Inconsolata-12"
		     "Consolas-12"))

(flucui-themes-load-style 'light)

;; Indent using spaces
(setq indent-tabs-mode nil)
(setq tab-width 4)

