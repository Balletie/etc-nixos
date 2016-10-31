(require 'exwm)
(require 'exwm-config)

;; Following is taken from (exwm-config-default), with some changes.
;; Set the initial workspace number.
(setq exwm-workspace-number 5)
(setq exwm-workspace-show-all-buffers t)
(setq exwm-layout-show-all-buffers t)

;; Make class name the buffer name
(add-hook 'exwm-update-class-hook
          (lambda ()
            (exwm-workspace-rename-buffer exwm-class-name)))
;; 's-r': Reset
(exwm-input-set-key (kbd "s-r") #'exwm-reset)
;; 's-w': Switch workspace
(exwm-input-set-key (kbd "s-w") #'exwm-workspace-switch)
;; 's-N': Switch to certain workspace
(dotimes (i 10)
  (exwm-input-set-key (kbd (format "s-%d" i))
		      `(lambda ()
			 (interactive)
			 (exwm-workspace-switch ,i))))
;; 's-p': Launch application
(exwm-input-set-key (kbd "s-p")
                    (lambda (command)
                      (interactive (list (read-shell-command "$ ")))
		      (start-process-shell-command command nil command)))

;; Line-editing shortcuts
(exwm-input-set-simulation-keys
 '(([?\C-b] . left)
   ([?\C-f] . right)
   ([?\C-p] . up)
   ([?\C-n] . down)
   ([?\C-a] . home)
   ([?\C-e] . end)
   ([?\M-v] . prior)
   ([?\C-v] . next)
   ([?\C-d] . delete)
   ([?\C-k] . (S-end delete))))
(exwm-config-misc)

(require 'exwm-systemtray)
(exwm-systemtray-enable)

(exwm-enable)
