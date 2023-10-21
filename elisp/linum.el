;; In emacs 29.1, linum mode is deprecated so we use
;; 'nlinum instead.

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
;; (add-hook 'org-mode-hook '(nlinum-mode nil))

;; relative line numbers
(setq display-line-numbers-type 'relative)
