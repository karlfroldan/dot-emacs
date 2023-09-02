(autoload 'zig-mode "zig-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.zig\\'" . zig-mode))

(if (>= emacs-major-version 28)
    (add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)
  (progn
    (defun colorize-compilation-buffer()
      (let ((inhibit-read-only t))
	(ansi-color-apply-on-region compilation-filter-start (point))))
    (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)))
