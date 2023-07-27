
(setq treesit-extra-load-path '("/home/zrg/.dotfiles/emacs/lang_def/"))

(use-package flycheck
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'flycheck-mode)
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)) ;; DO NOT treat emacs config file as a package file
  )

(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css?\\'" . css-mode))
  (add-to-list 'auto-mode-alist '("\\.hbs?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.php?\\'" . web-mode))
  (setq web-mode-engines-alist '(("ctemplate" . "\\.hbs\\'")))
  :custom
  (web-mode-enable-current-column-highlight nil)
  (web-mode-enable-current-element-highlight nil)
  )

(use-package emmet-mode
  :ensure t
  :config
  (add-hook 'web-mode-hook 'emmet-mode)
  (add-hook 'emmet-mode-hook (lambda () (setq emmet-indent-after-insert nil)))
  (add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2))) ;; indent 2 spaces.
  (add-to-list 'emmet-jsx-major-modes 'rjsx-mode)
  (add-hook 'rjsx-mode-hook 'emmet-mode)
  )
  
(use-package highlight-indent-guides
  :ensure t
  :diminish
  :config
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  (setq highlight-indent-guides-method 'character)
  )

(use-package rainbow-delimiters
  :ensure t
  :hook
  (prog-mode . rainbow-delimiters-mode)
  (prog-mode . show-paren-mode)
  )

(use-package smartparens
  :ensure t
  :hook
  (prog-mode . smartparens-mode)
  )

(use-package goto-line-preview
  :ensure t
  :commands goto-line-preview
  :bind
  ("C-l" . goto-line-preview)
  )

(use-package magit
  :ensure t
  :commands magit
  :bind ("C-x g" . magit)
  )

(use-package rjsx-mode
  :hook
  (eglot-ensure)
  :ensure t
  )

(use-package js2-mode
  :ensure t
  :config (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
          (add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-mode))
          (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
          (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-ts-mode))
          (setq js2-include-node-externs t)
	  (setq js-indent-level 2)
	  (setq js2-highlight-level 3)
	  )
(add-hook 'rjsx-mode-hook 'eglot-ensure)
(add-hook 'js2-mode-hook 'eglot-ensure)
(add-hook 'js-mode-hook 'eglot-ensure)
(add-hook 'typescript-ts-mode-hook 'eglot-ensure)

;; Add comments for different languages.
(defun addComment (choice)
  "Add helpful comments for different languages, acording to CHOICE "
  (interactive
   (list (completing-read "Choose: "
                          '(("js" . "js") ("bash" . "bash")) nil t)))
  (if (string= choice "js")
      (insert "
/**
 * Summary.
 *
 * Description.
 *
 * @since      x.x.x
 * @deprecated x.x.x Use new_function_name() instead.
 *
 * @param {type}   var           Description.
 * @param {type}   [var]         Description of optional variable.
 * @param {type}   [var=default] Description of optional variable with default variable.
 * @param {Object} objectVar     Description.
 * @param {type}   objectVar.key Description of a key in the objectVar parameter.
 *
 * @return {type} Return value description.
 */
")
      )
  (if (string= choice "bash")
      (insert "
: '

'
")
      )
  (message choice)
  choice)


