;;  ____
;; |_  /_ _ __ _
;;  / /| '_/ _` |   Sergiu Elmi
;; /___|_| \__, |   srj.elmi@gmail.com
;;         |___/
;;
;; . Emacs Configuration file
;; . . . . . . . . . . . . . . . . . . . . .

;; My Functions

(defun my/evil-shift-right ()
  (interactive)
  (evil-shift-right evil-visual-beginning evil-visual-end)
  (evil-normal-state)
  (evil-visual-restore))

(defun my/evil-shift-left ()
  (interactive)
  (evil-shift-left evil-visual-beginning evil-visual-end)
  (evil-normal-state)
  (evil-visual-restore))

(defun emacs-startup-screen ()
  "Display the weekly org-agenda and all todos."
  (org-agenda nil "n")
  (delete-other-windows))

(defun my-signature()
  (interactive)
  (insert-file-contents "~/.config/emacs/signature"))

;; Startup
(add-hook 'emacs-startup-hook #'emacs-startup-screen)
(setq inhibit-startup-message t)
;; Disable bars
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Disable lockfiles
(setq create-lockfiles nil)

;; Various
(set-fringe-mode 10) ;; margins
(setq visible-bell t)
(set-face-attribute 'default nil :font "Envy Code R")
(add-to-list 'custom-theme-load-path "~/.config/emacs/themes")
(global-auto-revert-mode t)
(global-visual-line-mode t)

;; Key-bindings
(global-set-key (kbd "<f10>") 'org-agenda)
;; just a function to display my ASCII art signature
(global-set-key (kbd "<f5>") 'my-signature)


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
; Indent 2 spaces in js2
(setq js-indent-level 2)
;; Display line-numbers
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'column-number-mode)
 
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

(use-package expand-region
  :bind ("C-=". 'er/expand-region))

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

  (evil-define-key 'visual global-map (kbd ">") 'my/evil-shift-right)
  (evil-define-key 'visual global-map (kbd "<") 'my/evil-shift-left)


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
  :config (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))) ;; DO NOT treat emacs config file as a package file

(use-package company
  :config (add-hook 'after-init-hook 'global-company-mode))

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

(use-package web-mode
  :config  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
	   (add-to-list 'auto-mode-alist '("\\.css?\\'" . css-mode))
	   (add-to-list 'auto-mode-alist '("\\.hbs?\\'" . web-mode))
	   (add-to-list 'auto-mode-alist '("\\.php?\\'" . web-mode))
	   (setq web-mode-engines-alist '(("ctemplate" . "\\.hbs\\'")))
  :custom  (web-mode-enable-current-column-highlight nil)
	   (web-mode-enable-current-element-highlight nil))

(use-package js2-mode
  :config (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
          (add-to-list 'auto-mode-alist '("\\.ts\\'" . js2-mode))
          (setq js2-include-node-externs t)
	  (setq js2-highlight-level 3)
	  )
(defvar lsp-enable-file-watchers nil) ; disable file watchers to bypass "too many files" error 

(use-package emmet-mode
  :config (add-hook 'web-mode-hook 'emmet-mode)
  (add-hook 'emmet-mode-hook (lambda () (setq emmet-indent-after-insert nil)))
  (add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2))) ;; indent 2 spaces.
  (add-hook 'rjsx-mode-hook 'emmet-mode))

(setq lsp-keymap-prefix "s-l")
(use-package lsp-mode
    :hook (python-mode . lsp-deferred) ;; make sure pyls is in path ~/.local/bin
    :hook (web-mode . lsp-deferred) 
    :hook (css-mode . lsp-deferred) 
    :hook (js2-mode . lsp-deferred) 
    :hook (js-jsx-mode . lsp-deferred) 
    :config (add-to-list 'lsp-language-id-configuration '(js-jsx-mode . "javascript"))
    :commands (lsp lsp-deferred))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config (setq lsp-ui-flycheck-enable t)
  	  (setq lsp-ui-sideline-show-flycheck t)
	  (setq lsp-ui-sideline-show-diagnostics t)
          (setq lsp-ui-sideline-show-code-actions t))

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

(use-package helpful
  :custom (counsel-describe-function-function #'helpful-callable)
          (counsel-describe-variable-function #'helpful-variable)
  :bind   ([remap describe-key] . helpful-key))

(use-package yascroll
  :config (global-yascroll-bar-mode 1))

(use-package magit)

(use-package neotree
  :config (global-set-key [f8] 'neotree-toggle)
	  (setq neo-window-fixed-size nil)
	  (setq neo-window-width 55)
	  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

;; ORG-mode
(setq org-directory "~/org/")
(setq org-agenda-files (directory-files-recursively "~/org/" "\\.org$"))
(defvar org-enforce-todo-dependencies t)

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
(defvar org-todo-keywords
    '((sequence "TODO(t!/!)" "BIROU(b!/!)" "BUG" "HOLD(h!/!)" "|" "DONE(d!/!)" "CANCELLED(c!/!)")))
(defvar org-todo-keyword-faces
    '(("TODO" .  "red" )
	("BIROU" .  "blue" )
	("HOLD" .  "orange" )
	("DONE" . "Green")
	("CANCELLED" .  "PaleGreen")))
(setq org-fontify-done-headline t)

;; PRIORITIES
(defvar org-highest-priority 49)
(defvar org-lowest-priority 57)
(defvar org-default-priority 53)

(use-package visual-fill-column
  :hook (org-mode . org-mode-visual-fill))

(use-package org-superstar
  ;; requires fontawesome font
  :after org
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-todo-bullet-alist '(("DONE" . ?)
                                     ("TODO" . ?)
                                     ("HOLD" . ?)
                                     ("CANCELLED" . ?)
                                     ("BIROU" . ?)
                                     ("BUG" . ?)
                                     ))
  (org-superstar-headline-bullets-list '(""))
  (org-superstar-special-todo-items t)
  (org-superstar-leading-bullet "  ")
  )

;; Set comments color to a nice green
(set-face-foreground 'font-lock-comment-face "spring green")
