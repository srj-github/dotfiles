;; Winner mode lets you remember windows history.
;; Use C-c arrows to undo and redo.
(winner-mode 1)

(use-package counsel
  :ensure t
  :config
  (counsel-mode)
  :diminish
  counsel-mode
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  )

(use-package ivy
  :ensure t
  :commands ivy-switch-buffer-other-window
  :defer 0.1
  :diminish
  :bind
  (("C-x B" . ivy-switch-buffer-other-window))
  :custom
  (ivy-count-format "%d/%d ")
  (ivy-use-virtual-buffers t)
  :config
  (ivy-mode 1)
  )

(use-package ivy-rich
  :ensure t
  :after ivy
  :init
  (ivy-rich-mode 1)
  :config
  (setq ivy-rich-parse-remote-buffer nil)
  )

(defun ivy-rich--switch-buffer-directory! (orig-fun &rest args)
  (cl-letf (((symbol-function 'directory-file-name) #'file-name-directory))
    (apply orig-fun args)))
(advice-add 'ivy-rich--switch-buffer-directory :around #'ivy-rich--switch-buffer-directory!)

(use-package swiper
  :ensure t
  :after ivy
  :bind
  (("C-s" . swiper))
  )

(use-package beacon
  :ensure t
  :diminish
  beacon-mode
  :config
  (beacon-mode 1)
  )

(use-package which-key
  :ensure t
  :diminish
  which-key-mode
  :config
  (which-key-mode 1)
  )

(use-package helpful
  :ensure t
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-key] . helpful-key)
    )

(use-package yascroll
  :ensure t
  :config (global-yascroll-bar-mode 1)
  )

(use-package diminish
  :ensure t
  :init
  (diminish 'visual-line-mode)
  (diminish 'org-indent-mode)
  )

(use-package company
  :ensure t
  :diminish
  company-mode
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  )
