;; -*- lexical-binding: t -*-

;; For any packages that integrate with an OS

(leaf alert
  :elpaca t
  :require t
  :config
  (if istermux
	  (setq alert-default-style 'termux)
	(setq alert-default-style 'libnotify))
  (when (eq system-type 'windows-nt)
    (alert-define-style
     'windows-desktop-notification-style
     :title "Windows Desktop Notification style"
     :notifier
     (lambda (info)
       (let ((notif-id (w32-notification-notify :title (plist-get info :title) :body (plist-get info :message))))
         ;; Close it after 3 seconds (no new notification can be sent if left unclosed)
         (run-with-timer 3 nil `(lambda() (w32-notification-close ,notif-id))))))
    (setq alert-default-style 'windows-desktop-notification-style))
  :bind ((:log4e-mode-map
		  ("q" . kill-current-buffer))))

(leaf prodigy
  :elpaca t)

(leaf edit-server
  :elpaca t
  :commands edit-server-start
  :setq
  (edit-server-new-frame . nil)
  :config
  (if after-init-time
	  (edit-server-start)
	(add-hook 'after-init-hook
			  #'(lambda nil
				  (edit-server-start)))))

;; TEL
(leaf TEL
  :when istermux
  :doc "setup for my TEL configuration"
  :setq
  (inhibit-startup-screen . t)
  :config
  (eshell))

(leaf windows
  :when iswindows
  :doc "the source of my anger issues"
  :config
  (defun wsl (command)
	"run shell-command in wsl"
	(interactive "sCommand: ")
	(async-shell-command (concat "wsl " command)))

  (defun wsl-path (path)
	"Convert a Windows path to a WSL-compatible path."
	(replace-regexp-in-string "C:" "/mnt/c"
							  (replace-regexp-in-string "\\\\" "/" path)))

  (defun run-in-wsl-dir (dir command)
	"Prompt for a directory DIR and a COMMAND to run in WSL."
	(interactive "DDirectory: 
sCommand: ")
	(wsl (format "cd %s && %s" (wsl-path dir) command)))

  (defun run-in-wsl-file (command)
	"Run a COMMAND in WSL on the current file."
	(interactive "sCommand: ")
	(let ((file (wsl-path buffer-file-name)))
      (wsl (format "%s %s" command file)))))


(leaf powershell
  :disabled (not iswindows)
  :elpaca t)

(leaf spookfox
  :doc "only works on linux"
  :disabled t
  :elpaca (spookfox :host github
					:repo "bitspook/spookfox"
					:files ("lisp/*.el" "lisp/apps/*.el"))
  :require t
  :config
  (setq spookfox-enabled-apps '(spookfox-org-tabs))
  (spookfox-init))

(leaf whicher
  :elpaca (whicher :host github
				   :repo "abo-abo/whicher"
				   :files ("*.el")))

(leaf define-word
  :elpaca (define-word :host github
					   :repo "abo-abo/define-word"
					   :files ("*.el")))

(leaf dmenu :elpaca t)

(leaf sudo-edit
  :after (embark)
  :bind
  (:embark-file-map
   ("s". sudo-edit-find-file))
  (:embark-become-file+buffer-map
   ("s" . sudo-edit-find-file)))

(leaf tsort
  :elpaca t)

(leaf kiss
  :elpaca (kiss :host github :repo "echawk/kiss.el"))

(provide 'k-os)
