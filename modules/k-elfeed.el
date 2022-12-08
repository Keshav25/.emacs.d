(leaf elfeed :bind
  (("C-x w" . elfeed))
  :config
  (with-eval-after-load 'elfeed
	(let
		((leaf--load-file-name "/home/haresh/.emacs.d/init.el"))
	  (setq elfeed-db-directory "~/.emacs.d/elfeed")
	  (setq elfeed-show-entry-switch 'display-buffer))))

;; Elfeed-Web
(leaf elfeed-web)

;; Elfeed-Org
(leaf elfeed-org
  :config
  (with-eval-after-load 'elfeed-org
	(setq elfeed-show-entry-switch 'display-buffer)
	(setq rmh-elfeed-org-files
		  (list "~/org/elfeed.org"))))

;; Elfeed For Youtube
(leaf elfeed-tube
  :after
  (elfeed-tube-setup)
  ;; :bind (:map elfeed-show-mode-map
  ;; 		 ("F" . elfeed-tube-fetch)
  ;; 		 ([remap save-buffer] . elfeed-tube-save)
  ;; 		 :map elfeed-seach-mode-map
  ;; 		 ("F" . efleed-tube-fetch)
  ;; 		 ([remap save-buffer] . elfeed-tube-save))
)

(leaf elfeed-tube-mpv
  ;; :bind (:map elfeed-show-mode-map
  ;; 			  ("C-c C-f" . elfeed-tube-mpv-follow-mode)
  ;; 			  ("C-c C-w" . elfeed-tube-mpv-where))
)

(provide 'k-elfeed)
