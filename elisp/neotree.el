(require 'neotree)

(global-set-key [f8] 'neotree-toggle)
(global-set-key [f7] 'neotree-show)

;; Every time when the neotree window is opened,
;; let it find the current file and jump to
;; the node in the tree
(setq neo-smart-open t)
