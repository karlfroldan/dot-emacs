;; Start MELPA
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;; Compatibility library for emacs < 24.3
(require 'cl-lib)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-sources
   '("~/.authinfo" "~/.authinfo.gpg" "~/.netrc" "~/.emacs.d/.authinfo.gpg"))
 '(custom-safe-themes
   '("063c278e83aa631e230535f1be093fa57d0df4a2f5b7e781c6952e6145532976" "db86c52e18460fe10e750759b9077333f9414ed456dc94473f9cf188b197bc74" "063c278e83aa631e230535f1be\012093fa57d0df4a2f5b7e781c6952e6145532976" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "90a6f96a4665a6a56e36dec873a15cbedf761c51ec08dd993d6604e32dd45940" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default))
 '(haskell-stylish-on-save t)
 '(ispell-dictionary nil)
 '(org-agenda-files '(concat (getenv "HOME") "/notes/contents.org"))
 '(org-format-latex-options
   '(:foreground default :background default :scale 1.3 :html-foreground "Black" :html-background "Transparent" :html-scale 1.3 :matchers
                 ("begin" "$1" "$" "$$" "\\(" "\\[")))
 '(package-selected-packages
   '(auxtex copilot avy corfu eglot-booster cargo-mode ggtags eglot-booster haskell yang-mode eat protobuf-mode counsel all-the-icons smart-mode-line-atom-one-dark-theme docker-compose-mode docker maxima cargo slint-mode slime org-ref org-ql bibtex-completion org-roam-bibtex rg helm-bibtex which-key material-theme deft emacsql-sqlite org-roam-ui org-roam cdlatex auctex all-the-icons-nerd-fonts all-the-icons-dired yasnippet-snippets auto-package-update yasnippet rust-mode dockerfile-mode smart-mode-line-powerline-theme smart-mode-line org-fragtog magit all-the-icons-ivy frog-jump-buffer projectile geiser-chicken ghci-completion yaml-mode use-package haskell-mode cmake-mode))
 '(package-vc-selected-packages
   '((eglot-booster :vc-backend Git :url "https://github.com/jdtsmith/eglot-booster"))))

(load (concat (getenv "HOME") "/.emacs.d/auxiliary_functions.el"))

;; Get the default settings for emacs.
(load-elisp-file "defaults.el")

;; GPG related stuff
(load-elisp-file "encryption.el")

;; Load all download packages from GNU elpa and melpa
(load-elisp-file "packages.el")

;; On some machines, I need to SSH but the IP addresses needed
;; to SSH may be different depending on the purpose of the machine.
(if (file-exists-p (relative-emacs-dir "ssh.el"))
    (load-elisp-file "ssh.el"))

;; Setting org-mode font
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-level-1 ((t (:inherit outline-1 :box nil :height 1.8 :foreground "#e6e6e6" :background nil))))
 '(org-level-2 ((t (:inherit outline-2 :box nil :height 1.6 :foreground "#e6e6e6" :background nil))))
 '(org-level-3 ((t (:inherit outline-3 :box nil :height 1.4 :foreground "#e6e6e6" :background nil))))
 '(org-level-4 ((t (:inherit outline-4 :box nil :height 1.2 :foreground "#e6e6e6" :background nil)))))

;; Load org-mode stuff
(load-elisp-file "org.el")
(load-elisp-file "irc.el")
(load-elisp-file "launcher.el")
(load-elisp-file "my-funs.el")
(load-elisp-file "languages.el")
(load-elisp-file "lsp.el")

;; Corfu support.
(use-package emacs
  :custom
  ;; Enable indentation+completion using TAB
  (tab-always-indent 'complete)
  ;; Hide commands in M-x which do not apply to the current mode.
  (read-extended-command-preidcate #'command-completion-default-include-p))
