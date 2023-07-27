;;  ____
;; |_  /_ _ __ _
;;  / /| '_/ _` |   Sergiu Elmi
;; /___|_| \__, |   srj.elmi@gmail.com
;;         |___/
;;
;; . Emacs Configuration file
;; .. created at: 2023-02-23
;; . . . . . . . . . . . . . . . . . . . . .

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(defun load-directory (directory)
  "Load recursively all `.el' files in DIRECTORY."
  (dolist (element (directory-files-and-attributes directory nil nil nil))
    (let* ((path (car element))
           (fullpath (concat directory "/" path))
           (isdir (car (cdr element)))
           (ignore-dir (or (string= path ".") (string= path ".."))))
      (cond
       ((and (eq isdir t) (not ignore-dir))
        (load-directory fullpath))
       ((and (eq isdir nil) (string= (substring path -3) ".el"))
        (load (file-name-sans-extension fullpath)))))))

;; Load Agenda at startup
(defun emacs-startup-screen ()
  "Display the weekly org-agenda and all todos."
  (org-agenda nil "n"))
(add-hook 'emacs-startup-hook #'emacs-startup-screen)
(setq initial-buffer-choice (lambda () (get-buffer-create "*Org Agenda*")))

;; Disable lockfiles
(setq create-lockfiles nil)

;; Various
(global-auto-revert-mode t) ;; Sync opened file if modified on disk.
(global-visual-line-mode t)
(setq-default truncate-lines t)

;; Key-bindings
(keymap-global-set "<f10>" 'org-agenda)

;; Set Custom file
(setq custom-file "~/.emacs.d/custom.el")

(setq temporary-file-directory "~/.emacs.d/tmp/")
(unless (file-exists-p temporary-file-directory)
    (make-directory temporary-file-directory))
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(load-directory "/home/zrg/.dotfiles/emacs/external")
(load-directory "/home/zrg/.dotfiles/emacs/init.d")
