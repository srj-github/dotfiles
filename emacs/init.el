;;  ____
;; |_  /_ _ __ _
;;  / /| '_/ _` |   Sergiu Elmi
;; /___|_| \__, |   srj.elmi@gmail.com
;;         |___/
;;
;; . Emacs Configuration file
;; . . . . . . . . . . . . . . . . . . . . .

(defun emacs-startup-screen ()
  "Display the weekly org-agenda and all todos."
  (org-agenda nil "n")
  (delete-other-windows))
(add-hook 'emacs-startup-hook #'emacs-startup-screen)

(setq inhibit-startup-message t)
(setq org-enforce-todo-dependencies t)
(setq lsp-enable-file-watchers nil) ; disable file watchers to bypass "too many files" error 
(scroll-bar-mode -1)
(tool-bar-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq visible-bell t)
(set-face-attribute 'default nil :font "Envy Code R")
(add-to-list 'custom-theme-load-path "~/.config/emacs/themes")

(global-auto-revert-mode t)

;; just a function to display my ASCII art signature
(global-set-key (kbd "<f5>") 'my-signature)
(defun my-signature()
  (interactive)
  (insert-file-contents "~/.config/emacs/signature"))

;; Display line-numbers
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'column-number-mode)

;;; Key-bindings

(global-set-key (kbd "<f10>") 'org-agenda)

;; Set Custom file
(setq custom-file "~/.config/emacs/custom.el")
(load custom-file)

;; Backup and Autosave Directories

(setq temporary-file-directory "~/.config/emacs/tmp/")
(unless (file-exists-p temporary-file-directory)
    (make-directory temporary-file-directory))

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))


;; Tab Indent to 4 spaces in python

(add-hook 'python-mode-hook
      (lambda ()
        (setq tab-width 4)
        (setq python-indent-offset 4)))
   
;; Scroll one line at a time (less "jumpy" than defaults)
(setq scroll-margin 1
  scroll-step 1
  mouse-wheel-scroll-amount '(1 ((shift) . 1))
  mouse-wheel-progressive-speed nil
  mouse-wheel-follow-mouse 't
  scroll-conservatively 10000
  scroll-preserve-screen-position 1)


;; MELPA initialization
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Use-package
(require 'use-package)
(setq use-package-always-ensure t)

(use-package exec-path-from-shell
  :config (when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize)))

(use-package diminish)

(use-package gruvbox-theme
  :config (load-theme 'gruvbox-dark-hard t))

(use-package counsel
  :config (counsel-mode))

(use-package ivy
  :defer 0.1
  :diminish
  :bind (("C-c C-r" . ivy-resume)
	 ("C-x B" . ivy-switch-buffer-other-window))
  :custom
  (ivy-count-format "%d/%d")
  (ivy-use-virtual-buffers t)
  :config (ivy-mode 1))

(use-package ivy-rich
  :init (ivy-rich-mode 1))

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)
	 ("C-r" . swiper)))

(use-package evil
  :bind ([remap evil-paste-pop] . counsel-yank-pop)
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-integration t)
  :config 
  (evil-mode 1)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'org-agenda-mode' 'normal)
  :demand)

(use-package evil-collection
  :after evil
  :custom (evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init))

(use-package evil-multiedit
  :bind (("M-r" . evil-multiedit-match-all)
	 ("M-d" . evil-multiedit-match-and-next)
	 ("M-D" . evil-multiedit-match-and-prev)))

(use-package flycheck
  :init (global-flycheck-mode)
  :config (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)) ;; DO NOT treat emacs config file as a package file
  )

(use-package company
  :config (add-hook 'after-init-hook 'global-company-mode))

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

(use-package web-mode
  :config  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
	   (add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
	   (setq web-mode-enable-current-column-highlight t)
	   (setq web-mode-enable-current-element-highlight t))

(use-package js2-mode
  :config (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
          (add-to-list 'auto-mode-alist '("\\.ts\\'" . js2-mode)))
(setq js2-include-node-externs t)

(use-package emmet-mode
  :config (add-hook 'web-mode-hook 'emmet-mode))

(setq lsp-keymap-prefix "s-l")
(use-package lsp-mode
    :hook (python-mode . lsp-deferred) ;; make sure pyls is in path ~/.local/bin
    :hook (web-mode . lsp-deferred) 
    :hook (js2-mode . lsp-deferred) 
    :commands (lsp lsp-deferred))

(use-package lsp-ui
  :custom lsp-ui-sideline-show-hover t
	  lsp-ui-sideline-show-diagnostics t
          lsp-ui-sideline-show-code-actions t)

(use-package highlight-indent-guides
  :diminish
  :config (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
          (setq highlight-indent-guides-method 'character))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :hook (prog-mode . show-paren-mode))

(use-package smartparens
  :hook (prog-mode . smartparens-mode))

(use-package doom-modeline
  :custom ((doom-modeline-minor-modes t)
	   (doom-modeline-enable-word-count t))
  :init (doom-modeline-mode 1))

(use-package beacon
  :config (beacon-mode 1))

(use-package goto-line-preview
  :bind ("C-l" . goto-line-preview))

(use-package which-key
  :config (which-key-mode 1))

;; (use-package helpful
;;   :custom (counsel-describe-function-function #'helpful-callable)
;;           (counsel-describe-variable-function #'helpful-variable)
;;   :bind   ([remap describe-key] . helpful-key))

(use-package yascroll
  :config (global-yascroll-bar-mode 1))

(use-package magit)

;; (use-package evil-magit
;;   :after magit)

;; ORG-mode
(setq org-directory "~/org/")
(setq org-agenda-files (directory-files-recursively "~/org/" "\\.org$"))

(add-hook 'org-mode-hook
	  (lambda ()
	    (org-indent-mode)
	    (setq org-ellipsis " ")
	    (setq org-hide-emphasis-markers nil)))

(defun org-mode-visual-fill ()
  (setq visual-fill-column-width 200
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

;; TODO STATES
(setq org-todo-keywords
    '((sequence "TODO(t!/!)" "BIROU(b!/!)" "HOLD(h!/!)" "|" "DONE(d!/!)" "CANCELLED(c!/!)")))
(setq org-todo-keyword-faces
    '(("TODO" .  "red" )
	("BIROU" .  "blue" )
	("HOLD" .  "orange" )
	("DONE" . "Green")
	("CANCELLED" .  "PaleGreen")))
(setq org-fontify-done-headline t)

;; PRIORITIES
(setq org-highest-priority 49)
(setq org-lowest-priority 57)
(setq org-default-priority 53)

(use-package visual-fill-column
  :hook (org-mode . org-mode-visual-fill))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode))
