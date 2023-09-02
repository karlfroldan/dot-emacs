(if (executable-find "csi")
    ;; If chicken scheme exists in this computer,
    ;; then set it as the default scheme interpreter.
    (setq scheme-program-name (executable-find "csi"))
  ())
