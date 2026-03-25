;; -*- lexical-binding: t -*-

(leaf elfeed
  :elpaca t
  :require t
  :custom
  (elfeed-db-directory . "~/.emacs.d/elfeed/")
  (elfeed-show-entry-switch . 'display-buffer)
  (elfeed-search-filter . "@1-month-ago +unread -shorts")
  (elfeed-search-title-max-width . 100)
  (elfeed-search-title-min-width . 20)
  (elfeed-search-trailing-width . 30)
  (elfeed-show-truncate-long-urls . t)
  (elfeed-search-remain-on-entry . t)
  (elfeed-sort-order . 'descending)
  :config

  (defun k/elfeed-display-buffer (buf &optional act)
    (pop-to-buffer buf)
    (set-window-text-height (get-buffer-window)
                            (round (* 0.7 (frame-height)))))

  (defun k/elfeed-search-show-entry-pre (&optional lines)
    (lambda (times)
      (interactive "p")
      (forward-line (* times (or lines 0)))
      (recenter)
      (call-interactively #'elfeed-search-show-entry)
      (select-window (previous-window))
      (unless elfeed-search-remain-on-entry (forward-line -1))))

  ;; YouTube Shorts filtering
  (defun k/elfeed-youtube-shorts-p (entry)
    (let ((url (elfeed-entry-link entry))
          (title (elfeed-entry-title entry)))
      (or
       (and url (string-match-p "/shorts/" url))
       (and title (string-match-p "#shorts?\\b" (downcase title))))))

  (defun k/elfeed-tag-youtube-shorts (entry)
    (when (k/elfeed-youtube-shorts-p entry)
      (elfeed-tag entry 'shorts)))

  (add-hook 'elfeed-new-entry-hook #'k/elfeed-tag-youtube-shorts)

  (defun k/elfeed-toggle-shorts ()
    (interactive)
    (if (string-match-p "-shorts" elfeed-search-filter)
        (elfeed-search-set-filter
         (replace-regexp-in-string " *-shorts" "" elfeed-search-filter))
      (elfeed-search-set-filter
       (concat elfeed-search-filter " -shorts"))))

  ;; YouTube helpers
  (defun k/elfeed-entry-youtube-p (entry)
    (let ((url (elfeed-entry-link entry)))
      (and url (string-match-p "\\(?:youtube\\.com\\|youtu\\.be\\)" url))))

  (defun k/elfeed-entry-youtube-id (entry)
    (let ((url (elfeed-entry-link entry)))
      (when (and url (string-match
                      "\\(?:v=\\|youtu\\.be/\\|/embed/\\|/v/\\)\\([A-Za-z0-9_-]\\{11\\}\\)"
                      url))
        (match-string 1 url))))

  (defun k/elfeed-auto-tag-youtube (entry)
    (when (k/elfeed-entry-youtube-p entry)
      (elfeed-tag entry 'youtube)))

  (add-hook 'elfeed-new-entry-hook #'k/elfeed-auto-tag-youtube)

  ;; Actions
  (defun k/elfeed-open-in-browser ()
    (interactive)
    (let ((entries (elfeed-search-selected)))
      (dolist (entry entries)
        (browse-url (elfeed-entry-link entry))
        (elfeed-untag entry 'unread))
      (mapc #'elfeed-search-update-entry entries)
      (unless (or elfeed-search-remain-on-entry (use-region-p))
        (forward-line))))

  (defun k/elfeed-open-in-eww ()
    (interactive)
    (let ((entry (if (eq major-mode 'elfeed-show-mode)
                     elfeed-show-entry
                   (elfeed-search-selected :single))))
      (when entry
        (eww (elfeed-entry-link entry)))))

  (defun k/elfeed-copy-link ()
    (interactive)
    (let ((entry (if (eq major-mode 'elfeed-show-mode)
                     elfeed-show-entry
                   (elfeed-search-selected :single))))
      (when entry
        (let ((link (elfeed-entry-link entry)))
          (kill-new link)
          (message "Copied: %s" link)))))

  ;; Star / bookmark
  (defun k/elfeed-toggle-star ()
    (interactive)
    (let ((entries (elfeed-search-selected)))
      (dolist (entry entries)
        (if (elfeed-tagged-p 'star entry)
            (elfeed-untag entry 'star)
          (elfeed-tag entry 'star)))
      (mapc #'elfeed-search-update-entry entries)
      (unless (or elfeed-search-remain-on-entry (use-region-p))
        (forward-line))))

  (defun k/elfeed-show-starred ()
    (interactive)
    (elfeed-search-set-filter "+star"))

  ;; Bulk
  (defun k/elfeed-mark-all-read ()
    (interactive)
    (when (y-or-n-p "Mark ALL visible as read? ")
      (mark-whole-buffer)
      (elfeed-search-untag-all-unread)))

  (defun k/elfeed-tag-selected (tag)
    (interactive (list (intern (completing-read "Tag: " (elfeed-db-get-all-tags)))))
    (let ((entries (elfeed-search-selected)))
      (dolist (entry entries)
        (elfeed-tag entry tag))
      (mapc #'elfeed-search-update-entry entries)
      (message "Tagged %d entries with +%s" (length entries) tag)))

  (defun k/elfeed-untag-selected (tag)
    (interactive (list (intern (completing-read "Untag: " (elfeed-db-get-all-tags)))))
    (let ((entries (elfeed-search-selected)))
      (dolist (entry entries)
        (elfeed-untag entry tag))
      (mapc #'elfeed-search-update-entry entries)
      (message "Removed -%s from %d entries" tag (length entries))))

  ;; Stats
  (defun k/elfeed-stats ()
    (interactive)
    (let ((total 0) (unread 0) (starred 0) (feeds 0))
      (with-elfeed-db-visit (entry feed)
        (cl-incf total)
        (when (elfeed-tagged-p 'unread entry)
          (cl-incf unread))
        (when (elfeed-tagged-p 'star entry)
          (cl-incf starred)))
      (setq feeds (hash-table-count (elfeed-db-feeds)))
      (message "Feeds: %d | Total: %d | Unread: %d | Starred: %d"
               feeds total unread starred)))

  ;; Custom search format with star indicator
  (defface k/elfeed-star-face
    '((t :foreground "#f0c040" :weight bold))
    "Face for starred entries.")

  (defun k/elfeed-search-format-entry (entry)
    (let* ((tags (elfeed-entry-tags entry))
           (starred (member 'star tags))
           (title (or (elfeed-meta entry :title) (elfeed-entry-title entry) ""))
           (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
           (feed (elfeed-entry-feed entry))
           (feed-title (when feed
                         (or (elfeed-meta feed :title)
                             (elfeed-feed-title feed))))
           (date (elfeed-search-format-date (elfeed-entry-date entry)))
           (tags-str (mapconcat #'symbol-name
                                (cl-remove-if (lambda (tag) (memq tag '(unread star)))
                                              tags)
                                ",")))
      (insert (propertize (if starred "★ " "  ") 'face 'k/elfeed-star-face))
      (insert (propertize date 'face 'elfeed-search-date-face) " ")
      (insert (propertize (elfeed-format-column
                           (or feed-title "") elfeed-search-trailing-width :left)
                          'face 'elfeed-search-feed-face) " ")
      (insert (propertize title 'face title-faces) " ")
      (when (not (string-empty-p tags-str))
        (insert (propertize (concat "(" tags-str ")")
                            'face 'elfeed-search-tag-face)))))

  (setq elfeed-search-print-entry-function #'k/elfeed-search-format-entry)

  ;; elfeed-tube-mpv supplementary commands
  ;; (primary playback is elfeed-tube-mpv, bound to 'p' below)
  (defun k/elfeed-play-audio-only ()
    (interactive)
    (let* ((entry (if (eq major-mode 'elfeed-show-mode)
                      elfeed-show-entry
                    (elfeed-search-selected :single)))
           (url (when entry (elfeed-entry-link entry))))
      (if url
          (progn
            (start-process "mpv-audio" nil "mpv" "--no-video" url)
            (message "Audio: %s" url))
        (message "No entry selected"))))

  (defun k/elfeed-mpv-speed-up ()
    (interactive)
    (when (and (fboundp 'mpv-live-p) (mpv-live-p))
      (let ((speed (mpv-get-property "speed")))
        (mpv-set-property "speed" (+ speed 0.25))
        (message "mpv: %.2fx" (+ speed 0.25)))))

  (defun k/elfeed-mpv-speed-down ()
    (interactive)
    (when (and (fboundp 'mpv-live-p) (mpv-live-p))
      (let ((speed (mpv-get-property "speed")))
        (mpv-set-property "speed" (max 0.25 (- speed 0.25)))
        (message "mpv: %.2fx" (max 0.25 (- speed 0.25))))))

  (defun k/elfeed-mpv-speed-reset ()
    (interactive)
    (when (and (fboundp 'mpv-live-p) (mpv-live-p))
      (mpv-set-property "speed" 1.0)
      (message "mpv: 1.00x")))

  (defun k/elfeed-mpv-toggle-pause ()
    (interactive)
    (if (and (fboundp 'mpv-live-p) (mpv-live-p))
        (mpv-pause)
      (message "mpv not running")))

  (defun k/elfeed-mpv-seek-forward ()
    (interactive)
    (when (and (fboundp 'mpv-live-p) (mpv-live-p))
      (mpv-seek 10)))

  (defun k/elfeed-mpv-seek-backward ()
    (interactive)
    (when (and (fboundp 'mpv-live-p) (mpv-live-p))
      (mpv-seek -10)))

  ;; Download
  (defvar k/elfeed-download-directory "~/Videos/elfeed/")

  (defun k/elfeed-download-video ()
    (interactive)
    (let* ((entry (if (eq major-mode 'elfeed-show-mode)
                      elfeed-show-entry
                    (elfeed-search-selected :single)))
           (url (when entry (elfeed-entry-link entry))))
      (if url
          (progn
            (make-directory k/elfeed-download-directory t)
            (let ((default-directory k/elfeed-download-directory))
              (async-shell-command
               (format "yt-dlp -f 'bestvideo[height<=1080]+bestaudio/best' -o '%%(title)s.%%(ext)s' '%s'"
                       url)))
            (message "Downloading to %s" k/elfeed-download-directory))
        (message "No entry selected"))))

  ;; Auto-update
  (defvar k/elfeed-update-timer nil)

  (defun k/elfeed-start-update-timer (&optional interval)
    (interactive)
    (k/elfeed-stop-update-timer)
    (setq k/elfeed-update-timer
          (run-at-time 0 (or interval (* 30 60)) #'elfeed-update))
    (message "Elfeed auto-update: every %d min" (/ (or interval 1800) 60)))

  (defun k/elfeed-stop-update-timer ()
    (interactive)
    (when k/elfeed-update-timer
      (cancel-timer k/elfeed-update-timer)
      (setq k/elfeed-update-timer nil)
      (message "Elfeed auto-update stopped")))

  :bind
  (("C-c o r" . elfeed))
  (:elfeed-search-mode-map
   ("S" . k/elfeed-toggle-shorts)
   ("m" . k/elfeed-toggle-star)
   ("M" . k/elfeed-show-starred)
   ("A" . k/elfeed-mark-all-read)
   ("I" . k/elfeed-stats)
   ("b" . k/elfeed-open-in-browser)
   ("e" . k/elfeed-open-in-eww)
   ("y" . k/elfeed-copy-link)
   ("P" . elfeed-tube-mpv)
   ("a" . k/elfeed-play-audio-only)
   ("d" . k/elfeed-download-video)
   ("+" . k/elfeed-tag-selected)
   ("-" . k/elfeed-untag-selected)
   ("T" . k/elfeed-start-update-timer))
  (:elfeed-show-mode-map
   ("r" . elfeed-update)
   ("w" . elfeed-show-yank)
   ("b" . k/elfeed-open-in-browser)
   ("e" . k/elfeed-open-in-eww)
   ("y" . k/elfeed-copy-link)
   ("P" . elfeed-tube-mpv)
   ("a" . k/elfeed-play-audio-only)
   ("SPC" . k/elfeed-mpv-toggle-pause)
   (">" . k/elfeed-mpv-speed-up)
   ("<" . k/elfeed-mpv-speed-down)
   ("0" . k/elfeed-mpv-speed-reset)
   ("}" . k/elfeed-mpv-seek-forward)
   ("{" . k/elfeed-mpv-seek-backward)
   ("d" . k/elfeed-download-video)
   ("m" . (lambda () (interactive)
            (when elfeed-show-entry
              (if (elfeed-tagged-p 'star elfeed-show-entry)
                  (elfeed-untag elfeed-show-entry 'star)
                (elfeed-tag elfeed-show-entry 'star))
              (elfeed-show-refresh))))))

(leaf relative-date
  :elpaca (relative-date :host github :url "https://github.com/rougier/relative-date/"))

(leaf nano-elfeed
  :after (elfeed elfeed-org)
  :elpaca (nano-elfeed :host github :repo "Keshav25/nano-elfeed"))

(leaf elfeed-org
  :elpaca t
  :after (org)
  :custom
  (rmh-elfeed-org-files . '("~/.emacs.d/elfeed.org"))
  :config
  (elfeed-org))

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
  :after (elfeed elfeed-tube)
  :elpaca t
  :require t
  :custom
  (elfeed-tube-mpv-options . '("--cache=yes"
                               "--force-window=yes"
                               "--ytdl-format=bestvideo[height<=?1080]+bestaudio/best"))
  :bind ((:elfeed-show-mode-map
          ("C-c C-f" . elfeed-tube-mpv-follow-mode)
          ("C-c C-w" . elfeed-tube-mpv-where))
         (:elfeed-search-mode-map
          ("C-c C-f" . elfeed-tube-mpv-follow-mode))))

(leaf elfeed-score
  :elpaca t
  :after elfeed
  :config
  (elfeed-score-enable)
  (let ((score-file "~/.emacs.d/elfeed.score"))
    (unless (file-exists-p score-file)
      (with-temp-file score-file
        (insert ";; Elfeed Score File -*- lisp -*-\n")
        (insert "(\"elfeed-score-serde-struct\"\n")
        (insert " :version 10\n")
        (insert " :title\n")
        (insert "  ((:text \"emacs\" :value 100 :type s)\n")
        (insert "   (:text \"linux\" :value 50 :type s)\n")
        (insert "   (:text \"tutorial\" :value 30 :type s))\n")
        (insert " :feed\n")
        (insert "  ((:text \"Sacha Chua\" :value 75 :type s :attr t))\n")
        (insert " :mark -500)\n")))
    (setq elfeed-score-serde-score-file score-file))
  :custom
  (elfeed-score-score-format . '("%d " 6 :right))
  :bind (:elfeed-search-mode-map
         ("=" . elfeed-score-set-score)
         ("E" . elfeed-score-maint-edit-score-file)))

(leaf eww
  :hook (eww-after-render-hook . eww-readable))

(leaf eww-lnum
  :elpaca t
  :bind (:eww-mode-map
         ("f" . eww-lnum-follow)
         ("F" . eww-lnum-universal)))

(leaf shrface
  :elpaca t
  :hook (eww-after-render-hook . shrface-mode))

(leaf elfeed-goodies
  :elpaca t
  :after elfeed
  :config
  (elfeed-goodies/setup))

(provide 'k-elfeed)
