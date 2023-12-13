(if (locate-file "sbcl" exec-path)
    (setq inferior-lisp-program "sbcl"))

;;(defun set-lisp-keys ()
;;  ((local-set-key (kbd "C-c C-q") #'slime-close-all-parens-at-sexp)))

;;(add-hook 'slime-mode-hook #'set-lisp-keys)
