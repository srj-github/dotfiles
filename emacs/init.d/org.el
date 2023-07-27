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

(setq org-directory "~/org")
(setq org-agenda-files (directory-files-recursively "~/org/" "\\.org$"))
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

;; PRIORITIES
(setq org-priority-lowest 67)
(setq org-priority-highest 1)
(setq org-priority-default 1)

(use-package visual-fill-column
  :ensure t
  :hook (org-mode . org-mode-visual-fill))

(use-package org-superstar
  :ensure t
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
