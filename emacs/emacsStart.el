;; This was used to start emacs with agenda buffer using emacsclient -c -e "$(cat /path/thisfile)"
(progn
 ;; (setq org-agenda-skip-function-global '(org-agenda-skip-entry-if 'todo '("BUG" "FEATURE")))
  (org-agenda "" "n" )
  (delete-other-windows)
)

