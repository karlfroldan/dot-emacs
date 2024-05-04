;;;; From https://www.reddit.com/r/unixporn/comments/s7p7pr/so_which_run_launcher_do_you_use_rofi_or_dmenu/

;;; This is still unusable so we use wofi for now.
;;; TODO: Make it so that only one emacs frame launches without the Messages buffer.
(defun my/emacs-run-launcher ()
  "Create and select a frame called emacs-app-launcher which consists
only of a minibuffer and has specific dimensions. Run counsel-linux-app
on that frame, which is an emacs command that prompts you to select an app and
open it in a dmenu like behavior. Delete the frame after that command has exited"
  (interactive)
  (with-selected-frame
      (make-frame '((name . "emacs-app-launcher")
                    (minibuffer . only)
                    (width . 120)
                    (height . 11)))
    (unwind-protect
        (counsel-linux-app)
      (delete-frame))))
