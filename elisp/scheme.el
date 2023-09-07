(if (executable-find "guile")
    ;; If chicken scheme exists in this computer,
    ;; then set it as the default scheme interpreter.
    (setq scheme-program-name (executable-find "guile"))
  ())

(use-package geiser)
(setq geiser-active-implementations '(guile))

;; open geiser-mode when scheme-mode is activated
(add-hook 'scheme-mode-hook 'geiser-mode)
