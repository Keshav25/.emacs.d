(leaf elfeed :elpaca t
  :custom
  :bind
  (("C-c o r" . elfeed))
  (:elfeed-show-mode-map
   ("r" . elfeed-update))
  :config
  (load-file "~/.emacs.d/elfeed-feeds.el")
  (setq elfeed-db-directory "~/.emacs.d/elfeed/")
  (setq elfeed-show-entry-switch 'display-buffer))

;; Elfeed-Web
;; (leaf elfeed-web :after elfeed :elpaca t)

;; Elfeed-Org
(leaf elfeed-org
  :elpaca t
  :config
  (with-eval-after-load 'elfeed-org
	(setq elfeed-show-entry-switch 'display-buffer)
	(setq rmh-elfeed-org-files
		  (list "~/org/elfeed.org"))))

;; Elfeed For Youtube
(leaf elfeed-tube
  :elpaca t
  :config
  (elfeed-tube-setup)
  :bind (:elfeed-show-mode-map
		 ("F" . elfeed-tube-fetch)
		 ([remap save-buffer] . elfeed-tube-save))
  (:elfeed-search-mode-map
   ("F" . elfeed-tube-fetch)
   ([remap save-buffer] . elfeed-tube-save)))

(leaf elfeed-tube-mpv
  :elpaca t
  :bind (:elfeed-show-mode-map
		 ("C-c C-f" . elfeed-tube-mpv-follow-mode)
		 ("C-c C-w" . elfeed-tube-mpv-where)))

(leaf eww
  :hook (eww-after-render-hook . eww-readable)
  :config
  (leaf shrface
	:elpaca t
	:hook (eww-mode . shrface-mode)))

(provide 'k-elfeed)
