(setq backup-directory-alist '(("." . "~/.bak.emacs")))

(require 'epa)

(setenv "GPG_AGENT_INFO" nil)

(setq my-gpg-key-email-address "karlfroldan@gmail.com")

(setq-default epa-file-select-keys '(my-gpg-key-email-address))
(setq epa-file-encrypt-to my-gpg-key-email-address)

(defmacro if-window (&rest, body)
  "Execute BODY only if emacs is running on window mode"
  `(unless (display-graphic-p)
     ,@body))

(defun relative-emacs-dir (rel-dir)
  "Gets the absolute directory from a path relative to emacs.d"
  (concat (getenv "HOME") "/.emacs.d/" rel-dir))

(defun package-recompile-all ()
  "Refresh and reinstall all activated packages."
  (interactive)
  (byte-recompile-directory package-user-dir nil))

(defun read-file-into-list (fname)
  "Read the contents of FNAME into a list of lines"
  (with-temp-buffer
    (insert-file-contents fname)
    (split-string (buffer-string) "\n" t)))

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
   '("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "90a6f96a4665a6a56e36dec873a15cbedf761c51ec08dd993d6604e32dd45940" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default))
 '(ispell-dictionary nil)
 '(org-agenda-files '(concat (getenv "HOME") "/notes/contents.org"))
 '(package-selected-packages
   '(which-key material-theme deft emacsql-sqlite org-roam-ui org-roam cdlatex auctex all-the-icons-nerd-fonts all-the-icons-dired slime pest-mode yasnippet-snippets auto-package-update yasnippet org-bullets rust-mode dockerfile-mode smart-mode-line-powerline-theme smart-mode-line org-fragtog magit scheme-complete all-the-icons-ivy frog-jump-buffer projectile geiser-guile geiser-chicken geiser ghci-completion yaml-mode lsp-haskell company lsp-mode use-package haskell-mode cmake-mode)))

(package-initialize)

(defun load-elisp-file (file-name)
  (load (concat (relative-emacs-dir file-name))))

(defun load-package (file-name)
  (load-elisp-file (concat "packages/" file-name ".el")))

(setq my-config-file-list (relative-emacs-dir "load-elisp-files"))

;; Get the config files that we will load into emacs.
(setq my-config-files
      (if (file-exists-p my-config-file-list)
          (read-file-into-list my-config-file-list)
        (mapcar #'file-name-sans-extension
                (directory-files (relative-emacs-dir "packages") nil "\\.el$"))))

;; If the file ~/.emacs.d/.bootstrapped exists, then don't continue the initialization.
(if (file-exists-p (relative-emacs-dir ".bootstrapped"))
    (mapc 'load-package my-config-files))

(defun bootstrap-emacs ()
  "Call on first emacs compile. This will install all packages"
  (interactive)
  (mapc #'package-install package-selected-packages)
  (all-the-icons-install-fonts)
  (write-region "" nil (relative-emacs-dir ".bootstrapped")))

;; Setting org-mode font
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-level-1 ((t (:inherit outline-1 :height 1.8 :foreground "#333333"))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.6 :foreground "#333333"))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.4 :foreground "#333333"))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.2 :foreground "#333333")))))

;; Load org-mode stuff
(mapc 'load (file-expand-wildcards (relative-emacs-dir "org/main.el")))

