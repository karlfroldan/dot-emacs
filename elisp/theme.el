(require 'cl-lib)
(defun font-candidate (&rest fonts)
    "Return existing font which first match."
    (cl-find-if (lambda (f) (find-font (font-spec :name f))) fonts))

(when (display-graphic-p)
    (set-face-attribute 'default nil
        :font
        (font-candidate
            '"Source Code Pro:size=14"
            "Inconsolata-12"
            "Consolas-12"))
    (load-theme 'flucui-light))

(use-package all-the-icons
    :if (display-graphic-p))

;; Add icons to dired mode
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
