;; For any packages that integrate with an OS
(leaf alert
  :ensure t
  :config
  (if istermux
	  (setq alert-default-style 'termux)
	(setq alert-default-style 'libnotify)))

(leaf prodigy
  :ensure t)

(leaf edit-server
  :ensure t
  :commands edit-server-start
  :setq
  (edit-server-new-frame . nil)
  :config
  (if after-init-time
	  (edit-server-start)
	(add-hook 'after-init-hook
			  #'(lambda nil
				  (edit-server-start)))))
  
