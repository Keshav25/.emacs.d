;; Dired
(leaf all-the-icons-dired)
(leaf dired-open)
(leaf peep-dired)
(leaf dirvish
  :config
  (dirvish-override-dired-mode 1))

;; Get file icons in dired
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
;; With dired-open plugin, you can launch external programs for certain extensions
;; For example, I set all .png files to open in 'sxiv' and all .mp4 files to open in 'mpv'
(setq dired-open-extensions '(("gif" . "sxiv")
                              ("jpg" . "sxiv")
                              ("png" . "sxiv")
                              ("mkv" . "mpv")
                              ("mp4" . "mpv")))

(provide 'k-dired)
