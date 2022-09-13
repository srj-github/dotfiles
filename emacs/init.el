;;  ____
;; |_  /_ _ __ _
;;  / /| '_/ _` |   Sergiu Elmi
;; /___|_| \__, |   srj.elmi@gmail.com
;;         |___/
;;
;; . Emacs Configuration file
;; . . . . . . . . . . . . . . . . . . . . .


(defvar zrg/pak-folder (expand-file-name "packages" user-emacs-directory) "Global var for the packages folder")
(add-to-list 'load-path zrg/pak-folder)

;;(require 'dired-x)

(winner-mode 1)

(defvar dired-sort-map (make-sparse-keymap))

;; (define-key dired-mode-map (kbd "C-c s") dired-sort-map)

;; (define-key dired-sort-map "s" (lambda () "sort by Size" (interactive) (dired-sort-other (concat dired-listing-switches " -S"))))
;; (define-key dired-sort-map "x" (lambda () "sort by eXtension" (interactive) (dired-sort-other (concat dired-listing-switches " -X"))))
;; (define-key dired-sort-map "t" (lambda () "sort by Time" (interactive) (dired-sort-other (concat dired-listing-switches " -t"))))
;; (define-key dired-sort-map "n" (lambda () "sort by Name" (interactive) (dired-sort-other dired-listing-switches)))
;; (define-key dired-sort-map "?" (lambda () "sort help" (interactive) (message "s Size; x eXtension; t Time; n Name")))

;; (provide 'dired-sort-map)

;; Startup

(setq inhibit-startup-message t)

(setq inhibit-default-init t)

;; Disable bars
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Disable lockfiles
(setq create-lockfiles nil)

;; Fix for xdg startup
(setq process-connection-type nil)

;; Various
(set-fringe-mode 10) ;; margins
(setq visible-bell t)
(set-face-attribute 'default nil :font "Envy Code R")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(global-auto-revert-mode t)
(global-visual-line-mode t)

;; Key-bindings
(global-set-key (kbd "<f10>") 'org-agenda)

;; Set Custom file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
;; Backup and Autosave Directories
(setq temporary-file-directory "~/.emacs.d/tmp/")
(unless (file-exists-p temporary-file-directory)
    (make-directory temporary-file-directory))
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

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
  (package-refresh-contents)
  )
(unless (package-installed-p 'use-package)
  (package-install 'use-package)
  )

;; Use-package
(require 'use-package)
(setq use-package-always-ensure t)

;; List with all the packages that are loaded below.
(defvar zrg/packages '(
		       "js2"
		       "evil"
		       "doom-modeline"
		       )
  )

(dolist (file zrg/packages)
  (if (or (file-exists-p(concat zrg/pak-folder "/" file "-zrg-setup.el"))
	  (file-exists-p(concat zrg/pak-folder "/" file "-zrg-setup.elc"))
	  )
      (require (intern (concat file "-zrg-setup")))
    (message (concat "Package " file "was removed"))
    )
  )

(use-package dashboard
  :config
  (setq dashboard-banner-logo-title "ZRG")
  (setq dashboard-startup-banner "~/.dotfiles/00_assets/logo_roundedCorners.png")
  (setq dashboard-center-content t)
  (setq dashboard-set-init-info t)
  (dashboard-setup-startup-hook)
  )

(use-package diminish
  :init
  (diminish 'visual-line-mode)
  (diminish 'org-indent-mode)
  )

(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox-dark-hard t)
  )

(use-package counsel
  :config
  (counsel-mode)
  :diminish
  counsel-mode
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  )

(use-package ivy
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
  :after ivy
  :init
  (ivy-rich-mode 1)
  :config
  (setq ivy-rich-parse-remote-buffer nil)
  )

(use-package swiper
  :after ivy
  :bind
  (("C-s" . swiper))
  )


(use-package dired
  :after evil
  :ensure
  nil
  :config
  (setq dired-dwim-target t)
  (setq delete-by-moving-to-trash t)
  (setq trash-directory "~/Trash")
  (setq dired-kill-when-opening-new-dired-buffer t)
  (setq global-auto-revert-non-file-buffers t)
  :commands
  (dired dired-jump)
  :bind
  (("C-x C-j" . dired-jump))
  :custom
  ((dired-listing-switches "-agho --group-directories-first"))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-up-directory
    "l" 'dired-find-alternate-file
    )
  )

(use-package sudo-edit)

(use-package dired-du
  :commands dired-du-mode
  :bind
  (
   ("s-z" . dired-du-mode)
   )
  :config
  (setq dired-du-size-format t)
  )

(use-package dired-open
  :after
  dired
  :config
  (add-to-list 'dired-open-functions #'dired-open-xdg t)
  )

(use-package all-the-icons-dired
  :hook
  (dired-mode . all-the-icons-dired-mode)
  )

(use-package dired-hide-dotfiles
  :after dired
  :hook
  (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map "H" 'dired-hide-dotfiles-mode)
  )

(use-package dired-rsync
  :after dired
  :config
  (bind-key "C-c C-r" 'dired-rsync dired-mode-map)
  )

(use-package flycheck
  :init
  (add-hook 'prog-mode-hook 'flycheck-mode)
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)) ;; DO NOT treat emacs config file as a package file
  ) 

(use-package flycheck-pos-tip
  :after
  flycheck
  :init
  (add-hook 'prog-mode-hook 'flycheck-pos-tip-mode)
  )

(use-package company
  :diminish
  company-mode
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  )

(use-package lsp-ivy
  :commands
  lsp-ivy-workspace-symbol
  )

(use-package web-mode
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

(defvar lsp-enable-file-watchers nil) ; disable file watchers to bypass "too many files" error 

(use-package emmet-mode
  :config
  (add-hook 'web-mode-hook 'emmet-mode)
  (add-hook 'emmet-mode-hook (lambda () (setq emmet-indent-after-insert nil)))
  (add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2))) ;; indent 2 spaces.
  (add-to-list 'emmet-jsx-major-modes 'rjsx-mode)
  (add-hook 'rjsx-mode-hook 'emmet-mode)
  )

(use-package lsp-mode
  :hook
  (python-mode . lsp-deferred) ;; make sure pyls is in path ~/.local/bin
  (web-mode . lsp-deferred) 
  (css-mode . lsp-deferred) 
  (js2-mode . lsp-deferred) 
  (js-jsx-mode . lsp-deferred) 
  :config
  (add-to-list 'lsp-language-id-configuration '(js-jsx-mode . "javascript"))
  (setq lsp-keymap-prefix "s-p")
  (setq lsp-pylsp-plugins-pydocstyle-ignore ["D100, D101, D102, D107"])
  :commands
  (lsp lsp-deferred)
  )

(use-package lsp-ui
  :hook
  (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-flycheck-enable t)
  (setq lsp-ui-sideline-show-flycheck t)
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-sideline-show-code-actions t)
  )

(use-package highlight-indent-guides
  :diminish
  :config
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  (setq highlight-indent-guides-method 'character)
  )

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode)
  (prog-mode . show-paren-mode)
  )

(use-package smartparens
  :hook
  (prog-mode . smartparens-mode)
  )

(use-package beacon
  :diminish
  beacon-mode
  :config
  (beacon-mode 1)
  )

(use-package goto-line-preview
  :commands goto-line-preview
  :bind
  ("C-l" . goto-line-preview)
  )

(use-package which-key
  :diminish
  which-key-mode
  :config
  (which-key-mode 1)
  )

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-key] . helpful-key)
	  )

(use-package yascroll
  :config (global-yascroll-bar-mode 1)
  )

(use-package magit
  :commands magit
  :bind ("C-x g" . magit)
  )

;; ORG-mode
(setq org-directory "~/org/GDT")
(setq org-agenda-files (directory-files-recursively "~/org/GDT/" "\\.org$"))
(setq org-archive-location "~/org/archive.org::* From %s")
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

;; BUG and FEATURE are used inside PROJECTS (sub headers) and are not displayed in (org-agenda n)
(defvar org-todo-keywords
    '((sequence "CHECK(k!/!)" "FEATURE(f!/!)" "PROJECT(p!/!)" "TASK(t!/!)" "BUG(u!/!)" "HOLD(h!/!)" "|" "DONE(d!/!)" "CANCELLED(c!/!)")))
(defvar org-todo-keyword-faces
    '(("FEATURE" .  "mediumPurple" )
	("CHECK" .  "DarkTurquoise" )
	("PROJECT" .  "MediumSeaGreen" )
	("TASK" .  "LightSeaGreen" )
	("HOLD" .  "orange" )
	("DONE" . "Green")
	("CANCELLED" .  "PaleGreen")))
(setq org-fontify-done-headline t)

;; PRIORITIES
(setq org-priority-lowest 67)
(setq org-priority-highest 1)
(setq org-priority-default 1)

(use-package visual-fill-column
  :hook (org-mode . org-mode-visual-fill))

(use-package org-superstar
  ;; requires fontawesome font
  :after org
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-todo-bullet-alist '(("DONE" . ?)
                                     ("FEATURE" . ?)
                                     ("PROJECT" . ?)
                                     ("TASK" . ?)
                                     ("CHECK" . ?)
                                     ("HOLD" . ?)
                                     ("CANCELLED" . ?)
                                     ("BUG" . ?)
                                     ))
  (org-superstar-headline-bullets-list '(""))
  (org-superstar-special-todo-items t)
  (org-superstar-leading-bullet "  ")
  )

;; Set comments color to a nice green
(set-face-foreground 'font-lock-comment-face "spring green")
(put 'dired-find-alternate-file 'disabled nil)
