;; For any packages that integrate with an OS
(leaf alert
  :ensure t
  :config
  (if istermux
	  (setq alert-default-style 'termux)
	(setq alert-default-style 'libnotify)))
