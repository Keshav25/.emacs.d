;; -*- lexical-binding: t -*-

(leaf elfeed
  :elpaca t
  :require t
  :custom
  (elfeed-db-directory . "~/.emacs.d/elfeed/")
  (elfeed-show-entry-switch . 'display-buffer)
  :config
  (defun browse-url-mpv (url &optional single)
	(start-process "mpv" nil "mpv" (shell-quote-argument url)))

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
	  (select-window (previous-window))
      (unless elfeed-search-remain-on-entry (forward-line -1))))
  :bind
  (("C-c o r" . elfeed))
  (:elfeed-show-mode-map
   ("r" . elfeed-update)
   ("w" . elfeed-show-yank)))

;; for nano-elfeed
(leaf relative-date
  :elpaca (relative-date :host github :url "https://github.com/rougier/relative-date/"))

(leaf nano-elfeed
  :after (elfeed elfeed-org)
  :elpaca (nano-elfeed :host github :repo "Keshav25/nano-elfeed"))

;; Elfeed-Org
(leaf elfeed-org
  :elpaca t
  :after (org)
  :custom
  (rhm-elfeed-org-files . '("~/.emacs.d/elfeed.org"))
  :config
  (elfeed-org))

;; Elfeed For Youtube
(leaf elfeed-tube
  :elpaca t
  :after (elfeed)
  :config
  (elfeed-tube-setup)
  :bind ((:elfeed-show-mode-map
		  ("F" . elfeed-tube-fetch)
		  ([remap save-buffer] . elfeed-tube-save))
		 (:elfeed-search-mode-map
		  ("F" . elfeed-tube-fetch)
		  ([remap save-buffer] . elfeed-tube-save))))

(leaf elfeed-tube-mpv
  :after elfeed
  :elpaca t
  :bind (:elfeed-show-mode-map
		 ("F" . elfeed-tube-fetch)
		 ("P" . elfeed-tube-mpv)
		 ("C-c C-f" . elfeed-tube-mpv-follow-mode)
		 ("C-c C-w" . elfeed-tube-mpv-where)))

(leaf eww
  :hook (eww-after-render-hook . eww-readable))

(leaf eww-lnum
  :elpaca t
  :bind (:eww-mode-map
		 ("f" . 'eww-lnum-follow)
		 ("F" . 'eww-lnum-universal)))

(leaf shrface
  :elpaca t
  :hook (eww-after-render-hook . shrface-mode))

(leaf elfeed-goodies
  :elpaca t)

(leaf emms
  :elpaca t
  :config
  (defun bard/play-elfeed-video ()
	"Play the URL of the entry at point in mpv if it's a YouTube video."
	(interactive)
	(let ((entry (elfeed-search-selected :single)))
      (if entry
          (let ((url (elfeed-entry-link entry)))
			(if (and url (string-match-p "https?://\\(www\\.\\)?youtube\\.com\\|youtu\\.be" url))
				(progn
                  (async-shell-command (format "mpv '%s'" url))
                  (elfeed-search-untag-all-unread))
              (message "The URL is not a YouTube link: %s" url)))
		(message "No entry selected in Elfeed."))))
  (defun bard/add-video-emms-queue ()
    "Play the URL of the entry at point in mpv if it's a YouTube video. Add it to EMMS queue."
    (interactive)
    (let ((entry (elfeed-search-selected :single)))
      (if entry
          (let ((url (elfeed-entry-link entry)))
            (if (and url (string-match-p "https?://\\(www\\.\\)?youtube\\.com\\|youtu\\.be" url))
                (let* ((playlist-name "Watch Later")
                       (playlist-buffer (get-buffer (format " *%s*" playlist-name))))
                  (unless playlist-buffer
                    (setq playlist-buffer (emms-playlist-new (format " *%s*" playlist-name))))
                  (emms-playlist-set-playlist-buffer playlist-buffer)
                  (emms-add-url url)
                  (elfeed-search-untag-all-unread)
                  (message "Added YouTube video to EMMS playlist: %s" url))
              (message "The URL is not a YouTube link: %s" url)))
        (message "No entry selected in Elfeed.")))))



(provide 'k-elfeed)
