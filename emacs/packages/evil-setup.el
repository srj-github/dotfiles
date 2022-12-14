(defun my/evil-shift-right ()
  (interactive)
  (evil-shift-right evil-visual-beginning evil-visual-end)
  (evil-normal-state)
  (evil-visual-restore)
  )

(defun my/evil-shift-left ()
  (interactive)
  (evil-shift-left evil-visual-beginning evil-visual-end)
  (evil-normal-state)
  (evil-visual-restore)
  )

(use-package evil
  :bind
  ([remap evil-paste-pop] . counsel-yank-pop)
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-integration t)
  (setq evil-auto-balance-windows nil) ;; Don't resize windows when creating new ones.
  :config 
  (evil-mode 1)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-define-key 'visual global-map (kbd ">") 'my/evil-shift-right)
  (evil-define-key 'visual global-map (kbd "<") 'my/evil-shift-left)

  (evil-set-initial-state 'org-agenda-mode' normal)
  :demand
  )

(use-package evil-collection
  :after
  evil
  :diminish
  evil-collection-unimpaired-mode
  :custom
  (evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init)
  )

(use-package evil-multiedit
  :bind
  (("M-r" . evil-multiedit-match-all)
   ("M-d" . evil-multiedit-match-and-next)
   ("M-D" . evil-multiedit-match-and-prev))
  )

(provide 'evil-setup)
