(require 'exwm)
(require 'exwm-config)

;; Following is taken from (exwm-config-default), with some changes.
;; Set the initial workspace number.
(setq exwm-workspace-number 5)
(setq exwm-workspace-show-all-buffers t)
(setq exwm-layout-show-all-buffers t)

;; Make class name the buffer name
(add-hook 'exwm-update-title-hook
	  (lambda ()
	    (exwm-workspace-rename-buffer
	     (truncate-string-to-width (format "%s : %s" exwm-class-name exwm-title)
				       43 0 nil "..."))))
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
;; 's-RET': Launch terminal
(exwm-input-set-key (kbd "<s-return>")
                    (lambda ()
		      (interactive)
		      (start-process-shell-command "urxvtc" nil "urxvtc")))

(exwm-config-misc)

(require 'exwm-systemtray)
(setq exwm-systemtray-height 15)
(exwm-systemtray-enable)

(exwm-enable)
