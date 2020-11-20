;;  ____
;; |_  /_ _ __ _
;;  / /| '_/ _` |
;; /___|_| \__, |
;;         |___/
;; . Emacs Configuration file
;; . . . . . . . . . . . . . . . . . . . . .

(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)
(setq visible-bell t)
(set-face-attribute 'default nil :font "Envy Code R" :height 130)
(add-to-list 'custom-theme-load-path "~/.config/emacs/themes")


;; just a function to display my ASCII art signature
(global-set-key (kbd "<f5>") 'my-signature)
(defun my-signature()
  (interactive)
  (insert-file-contents "~/.config/emacs/signature"))

;; Display line-numbers
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;;; Key-bindings

(global-set-key (kbd "<escape>") 'keyboard-escape-quit) ;; Use <esc> to escape
(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "<f12>") 'org-agenda)

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

;; Scroll one line at a time (less "jumpy" than defaults)
   
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; ORG-mode

(setq org-directory "~/org/")
(setq org-agenda-files (directory-files-recursively "~/org/" "\\.org$"))

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

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)
	 ("C-r" . swiper)))

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-integration t)
  :config (evil-mode 1))

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

(use-package emmet-mode
  :config (add-hook 'web-mode-hook 'emmet-mode))

(setq lsp-keymap-prefix "s-l")
(use-package lsp-mode
    :hook (python-mode . lsp-deferred) ;; make sure pyls is in path ~/.local/bin
    :hook (web-mode . lsp-deferred) 
    :hook (js2-mode . lsp-deferred) 
    :commands (lsp lsp-deferred))

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

(use-package dashboard
  :custom ((dashboard-center-content t)
	   (dashboard-set-heading-icons t)
	   (dashboard-set-file-icons t)
	   (dashboard-items '((bookmarks . 10)
			      (agenda . 20)))
	   (dashboard-week-agenda t))
  :config
  (dashboard-setup-startup-hook))
