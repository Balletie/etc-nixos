(require 'exwm)

;; Following is taken from (exwm-config-default), with some changes.
;; Set the initial workspace number.
(setq exwm-workspace-number 5)

(setq exwm-workspace-index-map (lambda (i) (nth i '("web " "prog" "cli " "edit" "misc"))))

(setq exwm-workspace-show-all-buffers t)
(setq exwm-layout-show-all-buffers t)

;; Hide or show mode-line depending on whether in line- or char-mode
(defun exwm-input-set-mode-line ()
  (with-current-buffer (window-buffer)
    (when (eq major-mode 'exwm-mode)
      (if (equal (nth 1 (nth 1 mode-line-process)) "line")
	  (exwm-layout-hide-mode-line)
	(exwm-layout-show-mode-line)))))

;; Add current exwm workspace to mode-line
(add-to-list 'mode-line-front-space
	     '(:eval (propertize
		      (apply exwm-workspace-index-map (list exwm-workspace-current-index))
		      'face 'mode-line-buffer-id)))

;; Make class name the buffer name
(add-hook 'exwm-update-title-hook
	  (lambda ()
	    (exwm-workspace-rename-buffer
	     (truncate-string-to-width (format "%s : %s" exwm-class-name exwm-title)
				       43 0 nil "..."))))

;;;; Keybindings
;;; Global keys
;; 's-r': Reset
(exwm-input-set-key (kbd "s-r") #'exwm-reset)
;; 's-w': Switch workspace
(exwm-input-set-key (kbd "s-w") #'exwm-workspace-switch)
;; 's-N': Switch to certain workspace
(dotimes (i 9)
  (exwm-input-set-key (kbd (format "s-%d" (1+ i)))
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

;;; Local keybindings (just in line-mode). Taken from
;;; https://github.com/carbohydratesn/spacemacs/blob/deea9318ff806ff4e530e3fcce3ea829b0a76d45/layers/%2Bwindow-management/exwm/packages.el
;; Press C-q to send key to X window.
(push ?\C-q exwm-input-prefix-keys)
(define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

;; Universal Get-me-outta-here
(push ?\C-g exwm-input-prefix-keys)
;; Universal Arguments
(push ?\C-u exwm-input-prefix-keys)
(push ?\C-0 exwm-input-prefix-keys)
(push ?\C-1 exwm-input-prefix-keys)
(push ?\C-2 exwm-input-prefix-keys)
(push ?\C-3 exwm-input-prefix-keys)
(push ?\C-4 exwm-input-prefix-keys)
(push ?\C-5 exwm-input-prefix-keys)
(push ?\C-6 exwm-input-prefix-keys)
(push ?\C-7 exwm-input-prefix-keys)
(push ?\C-8 exwm-input-prefix-keys)
(push ?\C-9 exwm-input-prefix-keys)
(delete ?\C-v exwm-input-prefix-keys)

(require 'exwm-systemtray)
(setq exwm-systemtray-height 15)
(exwm-systemtray-enable)

(exwm-enable)
