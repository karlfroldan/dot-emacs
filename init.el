(setq inhibit-startup-screen t)
(setq backup-directory-alist '(("." . "~/.bak.emacs")))

(defmacro if-window (&rest, body)
  "Execute BODY only if emacs is running on window mode"
  `(unless (display-graphic-p)
     ,@body))

(defun get-environment-variable (name)
  "Get the value of an environment variable by NAME."
  (let ((value (getenv name)))
    (if value
        value
      nil)))

(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu")
                         ("melpa" . "https://melpa.org/packages/")))

(defun all-true (lst)
  "Check if all items in the list are true"
  (if (null lst)
      t
    (and (car lst) (all-true (cdr lst)))))

(defun is-package-installed (pkg)
  "Check if the package is installed in the PC"
  (require pkg nil 'noerror))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "1930427eae3d4d830a43fd79fbda76021138b929c243a4e8606cf4f0531ea17c" "d548ac4bb4c8c0ba8f22476f5afcea11b7f1754065eefb118e1324f8a74883fb" "5642b25b6df4d6b63787cbc3d3ef07ca4cb7b0a7a00740ce8e9867c00e57632f" "15604b083d03519b0c2ed7b32da6d7b2dc2f6630bef62608def60cdcf9216184" "69f7e8101867cfac410e88140f8c51b4433b93680901bb0b52014144366a08c8" "88cb0f9c0c11dbb4c26a628d35eb9239d1cf580cfd28e332e654e7f58b4e721b" "3d94d6d1a1c23113a60c8496c9aed094dbc2695f219e8127bb168d17b1e6dab3" "21e3d55141186651571241c2ba3c665979d1e886f53b2e52411e9e96659132d4" "4b026ac68a1aa4d1a91879b64f54c2490b4ecad8b64de5b1865bca0addd053d9" "90a6f96a4665a6a56e36dec873a15cbedf761c51ec08dd993d6604e32dd45940" "f149d9986497e8877e0bd1981d1bef8c8a6d35be7d82cba193ad7e46f0989f6a" default))
 '(ispell-dictionary nil)
 '(package-selected-packages
   '(slime pest-mode yasnippet-snippets auto-package-update rjsx-mode js2-mode reason-mode psc-ide purescript-mode vterm idris-mode yasnippet org-bullets rust-mode dockerfile-mode smart-mode-line-powerline-theme smart-mode-line org-fragtog lua-mode minimap centaur-tabs solo-jazz-theme julia-mode magit modus-themes material-theme xcscope scheme-complete all-the-icons-ivy frog-jump-buffer projectile geiser-guile geiser-chicken geiser ghci-completion yaml-mode lsp-haskell company lsp-mode zig-mode use-package haskell-mode cmake-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-level-1 ((t (:inherit outline-1 :height 1.6))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.4))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.2))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.0)))))

(package-initialize)

;; We can install these packages
;; (mapc #'package-install package-selected-packages)

(defun my-load-file (name)
  (load (concat "~/.emacs.d/elisp/" name ".el")))

;; Install vterm later!!!
(setq my-config-files
      '("defaults"
        "c"
        "haskell"
        ;; "zig"
        "lsp"
        "latex"
        "lisp"
        ;; "scheme"
        "org"
        ;; "julia"
        ;; "lua"
        "magit"
        "ssh"
        "maxima"
        ;; "purescript"
        "yasnippet"
        ;; "erlang"
        "js"
        ))
(mapc 'my-load-file my-config-files)

(my-load-file "linum")

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

(when (display-graphic-p)
  (set-face-attribute 'default nil :font
                      (font-candidate
                       '"Source Code Pro:size=16"
                       "Inconsolata-12"
                       "Consolas-12"))
  (load-theme 'flucui-light))

;; Indent using spaces
(setq indent-tabs-mode nil)
(setq tab-width 4)

;; smart mode line
;; (setq sml/theme 'powerline-light)
(sml/setup)
(setq sml/extra-filler -6)

;; Function to recompile all packages
(defun package-recompile-all ()
  "Refresh and reinstall all activated packages."
  (byte-recompile-directory package-user-dir nil))

;; Single line scrolling\
(global-set-key (kbd "C-.") 'scroll-up-line)
(global-set-key (kbd "C-,") 'scroll-down-line)

;; Enable tree sitter for all supported major modes
(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line
