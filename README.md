## Emacs Personal Keybindings
#### General

<f5> : add signature (my-signature)
<f10> : org agenda (org-agenda)
C-= : select current word (er/expand-region)
C-x b : switch buffer (ivy-switch-buffer)
C-x B : switch buffer in other window (ivy-switch-buffer-other-window)
C-s : fancy search (swiper)
C-l : go to line (goto-line-preview)

> : move selection to right (evil-shit-right)
< : move selection to left (evil-shit-left)

M-d : find next match for current word or selection (evil-multiedit-match-and-next)
M-D : find previous match for current word or selection (evil-multiedit-match-and-prev)
M-r : match all for current word or selection (evil-multiedit-match-all)

#### Vterm

s-v : start vterm (vterm)
<f6> (in insert mode) : rename current buffer (rename-buffer)

#### Dired

H : show/hide hidden files (dired-hide-dotfiles-mode)
C-x C-j : start dired (dired-jump)
O : change file owner (dired-do-chown)
g G : change file group (dired-do-chgrp)
M : change file permissions (dired-do-chmod)

#### Org-mode

C-c C-x C-a : Archive heading (org-archive-subtree-default)
C-c C-x p : Add property (org-set-property)
C-c C-s : Add Schedule (org-schedule)
C-c C-d : Add Deadline (org-deadline)
C-c C-t : change TODO keyword (org-todo)
