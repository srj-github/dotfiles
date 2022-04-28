;;  ____
;; |_  /_ _ __ _
;;  / /| '_/ _` |   Sergiu Elmi
;; /___|_| \__, |   srj.elmi@gmail.com
;;         |___/
;;
;; . Emacs Configuration file
;; . . . . . . . . . . . . . . . . . . . . .

(require 'dired-x)

(defvar dired-sort-map (make-sparse-keymap))

(define-key dired-mode-map (kbd "C-c s") dired-sort-map)

(define-key dired-sort-map "s" (lambda () "sort by Size" (interactive) (dired-sort-other (concat dired-listing-switches " -S"))))
(define-key dired-sort-map "x" (lambda () "sort by eXtension" (interactive) (dired-sort-other (concat dired-listing-switches " -X"))))
(define-key dired-sort-map "t" (lambda () "sort by Time" (interactive) (dired-sort-other (concat dired-listing-switches " -t"))))
(define-key dired-sort-map "n" (lambda () "sort by Name" (interactive) (dired-sort-other dired-listing-switches)))
(define-key dired-sort-map "?" (lambda () "sort help" (interactive) (message "s Size; x eXtension; t Time; n Name")))

(provide 'dired-sort-map)

(defun emacs-startup-screen ()
;;  "Display the weekly org-agenda and all todos."
  (org-agenda nil "a")
  )

(defun my-signature()
  (interactive)
  (insert-file-contents "~/.emacs.d/signature"))

;; Startup

(defun zrg/buffer-to-side-window ()
  "Place the current buffer in the side window at the right."
  (interactive)
  (let ((buf (current-buffer)))
    (display-buffer-in-side-window
     buf '((window-width . 0.10)
           (side . right)
           (slot . -1)
           (window-parameters . (no-delete-other-windows . t))))
    (delete-window)))

(add-hook 'after-init-hook
	  (lambda ()

	    (find-file "~/org/start.org")
	    (zrg/buffer-to-side-window)
	    (other-window 1)
	    ))

;;(setq inhibit-startup-message t)

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
;; just a function to display my ASCII art signature
(global-set-key (kbd "<f5>") 'my-signature)

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

(defun efs/exwm-update-class ()
  (exwm-workspace-rename-buffer exwm-class-name))

(defun efs/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

;; This function should be used only after configuring autorandr!
;; Set the configuration for laptop screen only with "autorandr --save noExternalDisplay"
(defun efs/update-displays ()
  (efs/run-in-background "autorandr --change --force")
  (message  
   (string-trim (shell-command-to-string "autorandr --current")))
  )

(defun efs/exwm-init-hook ()

  ;; Show the time and date in modeline
  (setq display-time-day-and-date t)
  (display-time-mode 1)
  ;; Also take a look at display-time-format and format-time-string

  ;; Launch apps that will run in the background
  (efs/run-in-background "nm-applet")
  (efs/run-in-background "pasystray")
  (efs/run-in-background "blueman-applet")

  (display-battery-mode 1)

  (setq display-time-24hr-format t)
  (display-time-mode 1)
  

  (setq mouse-autoselect-window t
	focus-follows-mouse t)
  )

(use-package exwm
  :bind ("s-x" . 'counsel-linux-app)
  :config
  (require 'exwm-randr)
  (exwm-randr-enable)

  (add-hook 'exwm-randr-screen-change-hook
	    (lambda()
	      (sleep-for 5) ;; allow 5 seconds for the monitor to wake up
	      (if (string= (efs/update-displays) "noExternalDisplay")
		  (setq exwm-randr-workspace-monitor-plist '(0 "eDP-1" 1 "eDP-1" 2 "eDP-1" 3 "eDP-1" 4 "eDP-1" 5 "eDP-1" ))
		(setq exwm-randr-workspace-monitor-plist '(0 "HDMI-1" 1 "HDMI-1" 2 "HDMI-1" 3 "HDMI-1" 4 "HDMI-1" 5 "eDP-1"))
		)
	      ))

  (setq exwm-workspace-number 6)
  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

  ;; When window "class" updates, use it to set the buffer name
  (add-hook 'exwm-update-class-hook #'efs/exwm-update-class)

  ;; When EXWM starts up, do some extra confifuration
  (add-hook 'exwm-init-hook #'efs/exwm-init-hook)

  ;; (require 'exwm-systemtray)
  ;; (exwm-systemtray-enable)

  ;;(start-file-process-shell-command "firefox" nil "firefox")

  ;; Remap CapsLock to Ctrl
  (start-process-shell-command "xmodmap" nil "xmodmap ~/.dotfiles/emacs/Xmodmap")


  (setq exwm-input-prefix-keys
	'(
	  ?\C-w
	  ?\C-x
	  ?\M-x
	  )
	)
  (setq exwm-input-global-keys
	'(
          ;; Reset to line-mode (C-c C-k switches to char-mode via exwm-input-release-keyboard)
          ([?\s-r] . exwm-reset)

          ;; Switch workspace
          ([?\s-w] . exwm-workspace-switch)
          ([?\s-`] . (lambda () (interactive) (exwm-workspace-switch-create 0)))
          ([?\s-1] . (lambda () (interactive) (exwm-workspace-switch-create 1)))
          ([?\s-2] . (lambda () (interactive) (exwm-workspace-switch-create 2)))
          ([?\s-3] . (lambda () (interactive) (exwm-workspace-switch-create 3)))
          ([?\s-4] . (lambda () (interactive) (exwm-workspace-switch-create 4)))
          ([?\s-5] . (lambda () (interactive) (exwm-workspace-switch-create 5)))
	  ([M-tab] . next-buffer)

	  )
	)

  (exwm-enable)
  )

(use-package desktop-environment
  :after exwm
  :diminish desktop-environment-mode
  :config (desktop-environment-mode)
  :custom
  (desktop-environment-brightness-small-increment "2%+")
  (desktop-environment-brightness-small-decrement "2%-")
  (desktop-environment-brightness-normal-increment "5%+")
  (desktop-environment-brightness-normal-decrement "5%-"))

(use-package diminish
  :init
  (diminish 'visual-line-mode)
  )

(use-package gruvbox-theme
  :config (load-theme 'gruvbox-dark-hard t))

(use-package counsel
  :config (counsel-mode)
  :diminish counsel-mode
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  )

(use-package ivy
  :defer 0.1
  :diminish
  :bind (("C-x B" . ivy-switch-buffer-other-window))
  :custom
  (ivy-count-format "%d/%d")
  (ivy-use-virtual-buffers t)
  :config (ivy-mode 1))

(use-package ivy-rich
  :init (ivy-rich-mode 1))

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)))


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

  (evil-set-initial-state 'org-agenda-mode' normal)
  :demand)

(use-package evil-collection
  :after evil
  :diminish evil-collection-unimpaired-mode
  :custom (evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init))

(use-package evil-multiedit
  :bind (("M-r" . evil-multiedit-match-all)
	 ("M-d" . evil-multiedit-match-and-next)
	 ("M-D" . evil-multiedit-match-and-prev)))

(use-package dired
  :after evil
  :ensure nil
  :config (setq dired-dwim-target t)
          (setq delete-by-moving-to-trash t)
  :commands (dired dired-jump)
  :bind (
	 ("C-x C-j" . dired-jump)
	 )
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer))

(use-package dired-du
  :bind (("s-z" . dired-du-mode))
  :config
  (setq dired-du-size-format t)
  )

(use-package dired-open
  :after dired
  :config
  (add-to-list 'dired-open-functions #'dired-open-xdg t)
  )

(use-package dired-single
  :commands (dired dired-jump))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

(use-package flycheck
  :init (add-hook 'prog-mode-hook 'flycheck-mode)
  :config (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))) ;; DO NOT treat emacs config file as a package file

(use-package flycheck-pos-tip
  :after flycheck
  :init (add-hook 'prog-mode-hook 'flycheck-pos-tip-mode)
  )

(use-package company
  :diminish company-mode
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
  (add-to-list 'emmet-jsx-major-modes 'rjsx-mode)
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
  :diminish beacon-mode
  :config (beacon-mode 1))

(use-package goto-line-preview
  :bind ("C-l" . goto-line-preview))

(use-package which-key
  :diminish which-key-mode
  :config (which-key-mode 1))

(use-package helpful
  :custom (counsel-describe-function-function #'helpful-callable)
          (counsel-describe-variable-function #'helpful-variable)
  :bind   ([remap describe-key] . helpful-key))

(use-package yascroll
  :config (global-yascroll-bar-mode 1))

(use-package magit)

;; (use-package vterm
;;   :commands vterm
;;   :bind (("s-v" . vterm)
;; 	 ("<s-escape>" . vterm-send-escape)
;; 	 )
;;   :config
;;   (evil-define-key 'insert vterm-mode-map (kbd "<f6>") #'rename-buffer)
;;   (setq term-prompt-regexp "^[^#$%>\\n]*[#$%>] *")
;;   (setq vterm-shell "bash")
;;   (setq vterm-max-scrollback 10000)
;;   )

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
        visual-fill-column-center-text t)(use-package flycheck-tip
  :commands 'flycheck-tip-cycle
  :after flycheck
  :bind (:map flycheck-mode-map
	      ("C-c C-n" . flycheck-tip-cycle))
  )


  (visual-fill-column-mode 1))

;; TODO STATES
(defvar org-todo-keywords
    '((sequence "CHECK(k!/!)" "FEATURE(f!/!)" "BUG(u!/!)" "HOLD(h!/!)" "|" "DONE(d!/!)" "CANCELLED(c!/!)")))
(defvar org-todo-keyword-faces
    '(("FEATURE" .  "mediumPurple" )
	("CHECK" .  "DarkTurquoise" )
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
