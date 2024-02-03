(defun initialize-maxima (maxima-dir)
  (message "Initializing maxima")
  (add-to-list 'load-path (concat maxima-dir "/emacs"))
  ;;(autoload 'maxima-mode "maxima" "Maxima mode" t)
  ;;(autoload 'imaxima "Frontend for maxima with image support" t)
  ;;(autoload 'maxima "maxima" "Maxima interaction" t)
  ;;(autoload 'imath-mode "image" "Imath mode for math formula support" t)
  (setq imaxima-use-maxima-mode-flag t)
  (add-to-list 'auto-mode-alist '("\\.ma[cx]\\'" . maxima-mode ))
  (setq imaxima-scale-factor 1.4))

(let ((maxima-path (get-environment-variable "MAXIMA_DIR")))
  (if maxima-path
      (initialize-maxima maxima-path)
    (message "Is maxima installed?")))

;;(setq imaxima-scale-factor 1.3)
