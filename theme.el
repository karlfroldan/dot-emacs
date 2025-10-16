;;; THEME --- Theme configuration ---

(defun font-candidate (&rest fonts)
  "Return existing FONTS which first matches."
  (cl-find-if (lambda (f)
                (find-font (font-spec :name f))) fonts))

(my/load-make-after-frame
 (set-face-attribute 'default nil
                     :font (font-candidate '"Fira Code:size=13")))

(setq catppuccin-flavor 'mocha)
(load-theme 'catppuccin :no-confirm)
