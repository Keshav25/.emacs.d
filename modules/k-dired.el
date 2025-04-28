;; ;; Dired
(leaf dired
  :custom
  (dired-dwim-target . t)
  (dired-listing-switches . "-lgoAh --group-directories-first --time-style=long-iso")
  (dired-kill-when-opening-new-dired-buffer . t)
  (delete-by-moving-to-trash . t)
  :bind (:dired-mode-map
		 ("k" . dired-create-empty-file)
		 ("e" . wdired-change-to-wdired-mode)))

;; (leaf dired-sidebar
;;   :elpaca t
;;   :require t
;;   :bind ("C-S-d" . dired-sidebar-toggle-sidebar)
;;   :config
;;   (defun k/dired-sidebar-hooks ()
;; 	(unless (file-remote-p default-directory)
;; 	  (auto-revert-mode)))
;;   (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
;;   (push 'rotate-windows dired-sidebar-toggle-hidden-commands)
;;   (push 'ace-window dired-sidebar-toggle-hidden-commands)
;;   (setq dired-sidebar-use-term-integration t)
;;   :hook ((dired-sidebar-mode-hook . k/dired-sidebar-hooks)))

;; (leaf all-the-icons-dired :elpaca t
;; :hook ((dired-mode-hook . all-the-icons-dired-mode)
;; (dired-mode-hook . dired-hide-details-mode)))

;; (leaf diredfl
;;   :elpaca t
;;   :disabled t
;;   :config
;;   (diredfl-global-mode nil))

;; (leaf dired-subtree
;;   :elpaca t
;;   :bind (:dired-mode-map
;; 		 ("<mouse-1>" . dired-subtree-toggle)
;; 		 ("<tab>" . dired-subtree-toggle)))

;; (leaf dired-open :elpaca t)
;; (leaf peep-dired :elpaca t)
(leaf dirvish
  :elpaca t
  :config
  (dirvish-override-dired-mode)
  :hook
  (dirvish-setup-hook . dirvish-emerge-mode)
  :bind
  ("C-x d" . 'dirvish)
  ("C-S-d" . 'dirvish-side)
  (:dirvish-directory-view-mode-map
   ("q" . 'dirvish-quit)))

;; Get file icons in dired
;; (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
;; With dired-open plugin, you can launch external programs for certain extensions
;; For example, I set all .png files to open in 'sxiv' and all .mp4 files to open in 'mpv'
;; (setq dired-open-extensions '(("gif" . "sxiv")
;;                               ("jpg" . "sxiv")
;;                               ("png" . "sxiv")
;;                               ("mkv" . "mpv")
;;                               ("mp4" . "mpv")))

;; (leaf dired-preview
;;   :doc "disabled in favor of dirvish"
;;   :disabled t
;;   :elpaca t
;;   :config
;;   (dired-preview-global-mode 1))

(provide 'k-dired)
