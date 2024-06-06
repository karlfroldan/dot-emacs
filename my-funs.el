;;; package --- Summary
;;; Commentary:
;;; Some of my Emacs Lisp functions

;;; Code:

(defun make-shell (name)
  "Create a shell whose buffer name is NAME."
  (interactive "sName: ")
  (setq name (concat "$ "name))
  (eshell)
  (rename-buffer name))

(provide 'my-funs)
;;; my-funs.el ends here

