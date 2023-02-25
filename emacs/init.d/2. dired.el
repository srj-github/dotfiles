(defun dired-sort ()
  "Sort dired dir listing in different ways.
Prompt for a choice.
URL `http://ergoemacs.org/emacs/dired_sort.html'
Version 2015-07-30"
  (interactive)
  (let (-sort-by -arg)
    (setq -sort-by (ido-completing-read "Sort by:" '( "date" "size" "name" "dir")))
    (cond
     ((equal -sort-by "name") (setq -arg "-Al --si --time-style long-iso "))
     ((equal -sort-by "date") (setq -arg "-Al --si --time-style long-iso -t"))
     ((equal -sort-by "size") (setq -arg "-Al --si --time-style long-iso -S"))
     ((equal -sort-by "dir") (setq -arg "-Al --si --time-style long-iso --group-directories-first"))
     (t (error "logic error 09535" )))
    (dired-sort-other -arg )))

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
  (setq diredp-hide-details-initially-flag nil)

  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'diredp-up-directory-reuse-dir-buffer
    "l" 'diredp-find-file-reuse-dir-buffer
    (kbd "<return>") 'dired-open-xdg
    "s" 'dired-sort
    )
  :commands
  (dired dired-jump)
  :bind
  (("C-x C-j" . dired-jump))
  :custom
  (
   (dired-use-ls-dired t)
   (dired-listing-switches "-alh --group-directories-first")
   )
  )

(diredp-toggle-find-file-reuse-dir 1)

(use-package sudo-edit
  :ensure t
  )

(use-package dired-open
  :ensure t
  :after
  dired
  :config
  (add-to-list 'dired-open-functions #'dired-open-xdg t)
  )

(use-package dired-hide-dotfiles
  :ensure t
  :after dired
  :hook
  (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map "H" 'dired-hide-dotfiles-mode)
  )
