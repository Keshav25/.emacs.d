;; -*- lexical-binding: t -*-

(leaf elfeed
  :elpaca t
  :bind
  (("C-c o r" . elfeed))
  (:elfeed-show-mode-map
   ("r" . elfeed-update))
  (:elfeed-search-mode-map
   ("n" . (elfeed-search-show-entry-pre +1))
   ("p" . (elfeed-search-show-entry-pre -1))
   ("M-RET" . (elfeed-search-show-entry-pre)))
  :config
  (load-file "~/.emacs.d/elfeed-feeds.el")
  (setq elfeed-db-directory "~/.emacs.d/elfeed/")

  (defun browse-url-mpv (url &optional single)
	(start-process "mpv" nil "mpv" (shell-quote-argument url)))

  (setq elfeed-show-entry-switch 'switch-to-buffer-other-window)
  (defun elfeed-display-buffer (buf &optional act)
	(pop-to-buffer buf)
	(set-window-text-height (get-buffer-window)
							(round (* 0.7 (frame-height)))))

  (defun elfeed-search-show-entry-pre (&optional lines)
	"Returns a function to scroll forward or back in the Elfeed
  search results, displaying entries without switching to them."
    (lambda (times)
      (interactive "p")
      (forward-line (* times (or  lines 0)))
      (recenter)
      (call-interactively #'elfeed-search-show-entry)
	  q      (select-window (previous-window))
      (unless elfeed-search-remain-on-entry (forward-line -1)))))

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
