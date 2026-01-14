;; Start MELPA
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;; Compatibility library for emacs < 24.3
(require 'cl-lib)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(load (concat (getenv "HOME") "/.emacs.d/auxiliary_functions.el"))

;; Get the default settings for emacs.
(load-elisp-file "defaults.el")
(load-elisp-file "theme.el")

;; GPG related stuff
(load-elisp-file "encryption.el")

;; Load all download packages from GNU elpa and melpa
(load-elisp-file "packages.el")

;; Load org-mode stuff
; (load-elisp-file "org.el")
(load-elisp-file "my-funs.el")
(load-elisp-file "languages.el")
(load-elisp-file "lsp.el")

;; On some machines, I need to SSH but the IP addresses needed
;; to SSH may be different depending on the purpose of the machine.
(if (file-exists-p (relative-emacs-dir "ssh.el"))
    (load-elisp-file "ssh.el"))

