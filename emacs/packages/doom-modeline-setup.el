(use-package doom-modeline
  :config
  (setq doom-modeline-minor-modes t)
  (setq doom-modeline-enable-word-count nil)
  (setq doom-modeline-modal-icon t)
  (setq doom-modeline-height 10)
  (setq doom-modeline-hud t)
  (setq doom-modeline-buffer-encoding nil)
  :init
  (doom-modeline-mode 1)
  )


(provide 'doom-modeline-setup)
