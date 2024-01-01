(defun load-nano-config (config-name)
  (load (concat "~/.emacs.d/nano/nano-" config-name ".el")))

(setq load-configs '("base-colors"
                     "faces"
                     "writer"
                     "theme"
                     "theme-light"
                     "theme-dark"
                     "modeline"
                     "colors"))

(defun load-nano()
  "Load required nano only for org-mode"
  (add-to-list 'load-path "~/.emacs.d/nano")
  (load "~/.emacs.d/nano/nano.el")
  (require 'nano))

(setq nano-theme-var "dark"
      nano-font-size 10)

(load-nano)
(nano-toggle-theme)
