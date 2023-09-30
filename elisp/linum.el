(defmacro linum-mode-on (mode)
  "Enable linum-mode for a specific MODE."
  `(add-hook ',(intern (concat (symbol-name mode) "-mode-hook"))
	     (lambda () (linum-mode 1))))

;; Enable linum-mode for all those derived from prog-mode
(linum-mode-on prog)
(linum-mode-on text)

;;(add-hook 'org-mode-hook '(linum-mode nil))

;;(setq with-linum-mode
;;      '(emacs-lisp
;;	zig
;;	c
;;	sh
;;	julia
;;	scheme
;;	haskell))

;; We cannot directly operate on lists using mapc so
;; we use this instead.
;;(dolist (m with-linum-mode)
;;  (eval `(linum-mode-on ,m)))

;; relative line numbers
;;(setq display-line-numbers-type 'relative)
(linum-relative-mode)
