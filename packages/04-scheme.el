(if (executable-find "chicken")
    ;; If chicken scheme exists in this computer,
    ;; then set it as the default scheme interpreter.
    (setq scheme-program-name (executable-find "csi"))
  ())

(use-package geiser)
(setq geiser-active-implementations '(chicken))

;; open geiser-mode when scheme-mode is activated
(add-hook 'scheme-mode-hook 'geiser-mode)
