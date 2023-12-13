(require 'cl-lib)
(defun font-candidate (&rest fonts)
  "Return existing font which first match."
  (cl-find-if (lambda (f) (find-font (font-spec :name f))) fonts))

(when (display-graphic-p)
  (set-face-attribute 'default nil
                      :font
                      (font-candidate
                       '"Source Code Pro:size=16"
                       "Inconsolata-12"
                       "Consolas-12"))
  (load-theme 'flucui-light))
