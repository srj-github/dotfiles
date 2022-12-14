(defvar lspDocSwitch 1)
(defvar lspDocFocusSwitch 1)

;; Toggle lsp-ui object documentation at cursor.
(defun toggleLspDoc ()
  (interactive)
  (catch 'return
    (cond ((eq lspDocSwitch 1)
	   (call-interactively 'lsp-ui-doc-show)
	   (setq  lspDocSwitch 2)
	   (throw 'return "End function")
	   ))
    (cond ((eq lspDocSwitch 2)
	   (call-interactively 'lsp-ui-doc-unfocus-frame) ;; remove focus if cursor is inside.
	   (setq lspDocFocusSwitch 1) ;; reset focus variable.
	   (setq lspDocSwitch 1)
	   (call-interactively 'lsp-ui-doc-hide)
	   ))
    )
  )

;; Toggle lsp-ui documentation focus.
;; This will move the cursor inside the documentation window, or out of it if called again.
(defun toggleLspDocFocus ()
  (interactive)
  (catch 'return
    (cond ((eq lspDocSwitch 2)
	   (cond ((eq lspDocFocusSwitch 1)
		  (call-interactively 'lsp-ui-doc-focus-frame)
		  (setq  lspDocFocusSwitch 2)
		  (throw 'return "End function")
		  ))
	   (cond ((eq lspDocFocusSwitch 2)
		  (setq lspDocFocusSwitch 1)
		  (call-interactively 'lsp-ui-doc-unfocus-frame)
		  ))
	   ))
    )
  )

(use-package lsp-ui
  :hook
  (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-flycheck-enable t)
  (setq lsp-ui-sideline-show-flycheck t)
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-sideline-show-code-actions t)
  :bind
  ("C-c d" . toggleLspDoc)
  ("C-c f" . toggleLspDocFocus)
  )

(provide 'lsp-ui-setup)
