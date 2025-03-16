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

;; (setq modus-themes-italic-constructs t ; Allow italics in code
;;       modus-themes-mode-line '(borderless)
;;       modus-themes-variable-pitch-ui nil
;;       modus-themes-disable-other-themes t
;;       modus-themes-prompts '(italic)

;;       ;; Keep mode-line border but make it the same color as the background
;;       ;; we also wanna keep the fringe invisible
;;       modus-themes-common-palette-overrides
;;       '((bg-mode-line-active bg-blue-intense)
;;         (fg-mode-line-active fg-main)
;;         (border-mode-line-active cyan-subtle)

;;         ;; Invisible fringe
;;         (fringe unspecified)

;;         ;; Inactive border themes
;;         (bg-mode-line-inactifve bg-cyan-subtle)
;;         (fg-mode-line-inactive fg-main)
;;         (border-mode-line-inactive blue-subtle)

;;         ;; Line numbers
;;         (bg-line-number-active bg-cyan-intense)

;;         ;; Mouse highlights
;;         (cursor fg-main)
;;         (bg-hover bg-blue-subtle)))

;; (load-theme 'modus-vivendi-tinted)
