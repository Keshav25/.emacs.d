(leaf dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  :setq
  (initial-buffer-choice . (lambda () (get-buffer-create "*dashboard*")))
  (dashboard-center-content . t)
  (dashboard-show-shortcuts . t)
  (dashboard-items .'((recents . 5)
					  (bookmarks . 5)
					  (agenda . 5)
					  (registers . 5)))
  (dashboard-set-heading-icons . t)
  (dashboard-set-file-icons . t)
  (dashboard-set-navigator . t)
  (dashboard-week-agenda . t)
  (dashboard-filter-agenda-entry . 'dashboard-no-filter-agenda)
  :config
  (setq dashboard-banner-logo-title (concat "Welcome " user-full-name))
  (leaf org-dashboard :ensure t)
  (leaf dashboard-ls :ensure t)
  (leaf elfeed-dashboard :ensure t)
  (leaf dashboard-hackernews :ensure t))

(provide 'k-dashboard)
