;; ;; Dired
(leaf dired
  :bind (:dired-mode-map
		 ("k" . dired-create-empty-file)))

(leaf dired-sidebar
  :ensure t
  :require t
  :bind ("C-S-d" . dired-sidebar-toggle-sidebar)
  :config
  (defun k/dired-sidebar-hooks ()
	(unless (file-remote-p default-directory)
	  (auto-revert-mode)))
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)
  (push 'ace-window dired-sidebar-toggle-hidden-commands)
  (setq dired-sidebar-use-term-integration t)
  :hook ((dired-sidebar-mode-hook . k/dired-sidebar-hooks)))

(leaf all-the-icons-dired :ensure t
  :hook ((dired-mode-hook . all-the-icons-dired-mode)
		 (dired-mode-hook . dired-hide-details-mode)))

(leaf diredfl
  :ensure t
  :config
  (diredfl-global-mode t))

(leaf dired-subtree
  :ensure t
  :bind (:dired-mode-map
		 ("<tab>" . dired-subtree-toggle)))

(leaf dired-open :ensure t)
;; (leaf peep-dired :ensure t)
(leaf dirvish
  :ensure t
  :config
  (dirvish-override-dired-mode)
  :bind (:dirvish-directory-view-mode-map
		 ("q" . 'dirvish-quit)))

;; Get file icons in dired
;; (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
;; With dired-open plugin, you can launch external programs for certain extensions
;; For example, I set all .png files to open in 'sxiv' and all .mp4 files to open in 'mpv'
(setq dired-open-extensions '(("gif" . "sxiv")
                              ("jpg" . "sxiv")
                              ("png" . "sxiv")
                              ("mkv" . "mpv")
                              ("mp4" . "mpv")))

(leaf dired-preview
  :doc "disabled in favor of dirvish"
  :disabled t
  :ensure t
  :config
  (dired-preview-global-mode 1))

(provide 'k-dired)
