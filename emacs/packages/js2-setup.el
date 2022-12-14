(use-package js2-mode
  :config (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
          (add-to-list 'auto-mode-alist '("\\.ts\\'" . js2-mode))
          (setq js2-include-node-externs t)
	  (setq js-indent-level 2)
	  (setq js2-highlight-level 3)
	  )

(provide 'js2-setup)
