;;; THEME --- Theme configuration ---

(defun font-candidate (&rest fonts)
  "Return existing FONTS which first matches."
  (cl-find-if (lambda (f)
                (find-font (font-spec :name f))) fonts))

(my/load-make-after-frame
 (set-face-attribute 'default nil
                     :font (font-candidate '"Fira Code:size=12")))

(use-package modus-themes
  :config
  (setq-default modus-themes-italic-constructs t
                modus-themes-prompts '(italic)
                modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tinted)
                modus-themes-variable-pitch-ui t)
   
  (load-theme 'modus-vivendi)
  (modus-themes-load-theme 'modus-vivendi-tinted))
