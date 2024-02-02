(setq backup-directory-alist '(("." . "~/.bak.emacs")))

(require 'epa)

(setenv "GPG_AGENT_INFO" nil)
(setq-default epa-file-select-keys '("karlfroldan@proton.me"))
(setq epa-file-encrypt-to "karlfroldan@proton.me")

(defmacro if-window (&rest, body)
  "Execute BODY only if emacs is running on window mode"
  `(unless (display-graphic-p)
     ,@body))

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
   '("90a6f96a4665a6a56e36dec873a15cbedf761c51ec08dd993d6604e32dd45940" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default))
 '(ispell-dictionary nil)
 '(org-agenda-files '("~/notes/contents.org"))
 '(package-selected-packages
   '(material-theme deft emacsql-sqlite org-roam-ui org-roam cdlatex auctex all-the-icons-nerd-fonts all-the-icons-dired slime pest-mode yasnippet-snippets auto-package-update yasnippet org-bullets rust-mode dockerfile-mode smart-mode-line-powerline-theme smart-mode-line org-fragtog magit scheme-complete all-the-icons-ivy frog-jump-buffer projectile geiser-guile geiser-chicken geiser ghci-completion yaml-mode lsp-haskell company lsp-mode use-package haskell-mode cmake-mode)))

(package-initialize)

(defun my-load-file (name)
  (load (concat "~/.emacs.d/elisp/" name ".el")))

(setq my-config-file-list "~/.emacs.d/load-elisp-files")

;; Get the config files that we will load into emacs.
(setq my-config-files
      (if (file-exists-p my-config-file-list)
          (read-file-into-list my-config-file-list)
        (mapcar #'file-name-sans-extension
                (directory-files "~/.emacs.d/elisp" nil "\\.el$"))))

;; If the file ~/.emacs.d/.bootstrapped exists, then don't continue the initialization.
(if (file-exists-p "~/.emacs.d/.bootstrapped")
    (mapc 'my-load-file my-config-files))

(defun bootstrap-emacs ()
  "Call on first emacs compile. This will install all packages"
  (interactive)
  (mapc #'package-install package-selected-packages)
  (all-the-icons-install-fonts)
  (write-region "" nil "~/.emacs.d/.bootstrapped"))

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
(mapc 'load (file-expand-wildcards "~/.emacs.d/org/*.el"))

