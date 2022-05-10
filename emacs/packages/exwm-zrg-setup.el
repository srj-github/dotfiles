(defun efs/exwm-update-class ()
  (exwm-workspace-rename-buffer exwm-class-name)
  )

(defun efs/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts))))
  )

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


  (start-process "terminator" nil "terminator")
  (start-process "thunderbird" nil "thunderbird")

  ;; Create second workspace for coding
  (exwm-workspace-switch 1)
  ;;(start-process "firefox" nil "firefox")
  (evil-window-vsplit)
  (evil-window-right 1)
  (dired "~/code")
  (evil-window-split)
  (evil-window-down 1)
  (funcall 'bash-term)
  (exwm-layout-shrink-window 450)
  (exwm-workspace-switch 0)
  )

(defun efs/configure-window-by-class ()
  (interactive)
  (pcase exwm-class-name
    ("Terminator" (exwm-workspace-move-window 5))
    ("Thunderbird" (exwm-workspace-move-window 2))
    )
  )

  (defun efs/exwm-update-title ()
    (pcase exwm-class-name
      ("firefox" (exwm-workspace-rename-buffer (format "Firefox: %s" exwm-title)))))

(use-package exwm
  :config
  (require 'exwm-randr)
  (exwm-randr-enable)

  ;; Make external display get workspaces if plugged in, or laptop monitor if not.
  (add-hook 'exwm-randr-screen-change-hook
	    (lambda()
	      (if (string= (efs/update-displays) "noExternalDisplay")
		  (setq exwm-randr-workspace-monitor-plist '(0 "eDP-1" 1 "eDP-1" 2 "eDP-1" 3 "eDP-1" 4 "eDP-1" 5 "eDP-1" ))
		(setq exwm-randr-workspace-monitor-plist '(0 "HDMI-1" 1 "HDMI-1" 2 "HDMI-1" 3 "HDMI-1" 4 "HDMI-1" 5 "eDP-1"))
		)
	      (exwm-randr-refresh)
	      ))


  (setq exwm-workspace-number 6)
  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

  ;; When window "class" updates, use it to set the buffer name
  (add-hook 'exwm-update-class-hook #'efs/exwm-update-class)

  ;; Configure windows as they're created
  (add-hook 'exwm-manage-finish-hook #'efs/configure-window-by-class)

  ;; When EXWM starts up, do some extra confifuration
  (add-hook 'exwm-init-hook #'efs/exwm-init-hook)

  ;; When window "class" updates, use it to set the buffer name
  (add-hook 'exwm-update-class-hook #'efs/exwm-update-class)

  ;; When window title updates, use it to set the buffer name
  (add-hook 'exwm-update-title-hook #'efs/exwm-update-title)

  (require 'exwm-systemtray)
  (exwm-systemtray-enable)

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
	  ([?\s-t] . bash-term)
	  ([?\s-x] . counsel-linux-app)

	  )
	)

  (exwm-enable)
  )

(provide 'exwm-zrg-setup)
