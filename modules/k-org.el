(leaf org
  :bind (("C-c a" . org-agenda)
		 ("C-c c" . org-capture))
  :setq
  (org-ellipsis . " ▾")
  (org-startup-numerated . 1)
  (org-startup-truncated . 1)
  (org-startup-indented . 1)
  (org-appear-mode . 1)
  (org-modern-mode . 1)
  (org-use-speed-commands . 1)
  :custom
  (org-directory . "~/org")
  (org-agenda-files . '("~/org/compsol.org"
						"~/org/school.org"
						"~/org/daily.org"
						"~/org/inbox.org"))
  (org-capture-templates
   .
   '(
	 ("p" "Protocol" entry (file+headline "inbox.org" "Inbox")
	  "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
	 ("L" "Protocol Link" entry (file+headline "inbox.org" "Inbox")
	  "* %? %:annotation\n")
	 ("i" "inbox" entry
	  (file "~/org/inbox.org")
	  "* %?\n %T\n %a\n %i\n"
	  :empty-lines 1 )
	 ("c" "compsol"
	  entry (file "~/org/compsol.org")
	  "* %?\n %T\n %i\n"
	  :empty-lines 1)
	 ("s" "school"
	  entry (file "~/org/school.org")
	  "* %?\n %T\n %i\n"
	  :empty-lines 1)
	 ("f" "food"
	  entry (file "~/org/food-journal.org")
	  "* %?\n %T\n %i\n"
	  :empty-lines 1)
	 ("d" "daily-template"
	  entry
	  (file+olp+datetree "daily.org")
	  "%[~/org/daily-template]"
	  ;; :unnarrowed 1
	  :tree-type week
	  ;; :time-prompt t
	  )))
  :init
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t))
   ))

;; https://tech.toryanderson.com/2020/08/18/orgmode-system-notifications-with-dunst/
;; (setq appt-display-format 'window)

;; (setq appt-disp-window-function (function tsa/appt-disp-window))
;; (defun tsa/appt-disp-window (min-to-app new-time msg)
;;   (save-window-excursion
;;     (shell-command
;;      (concat
;;       "notify-send \"Orgmode: " msg "\"") nil nil)))

;; Org-Journal
(leaf org-journal
  :bind
  ("C-c C-j" . org-journal-new-entry))

;; Evil Org
(leaf evil-org
  :after org
  :hook (org-mode . (lambda () (evil-org-mode)))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  :setq (evil-want-C-i-jump . nil))

;; Org-Noter
(leaf org-noter)
(leaf org-noter-pdftools)

;; To fix in the future
;; (use-package org-krita
;;   :ensure t
;;   :quelpa (org-krita :fetcher github :repo "lepisma/org-krita" :files ("*.el" "resources"))
;;   :config
;;   (add-hook 'org-mode-hook 'org-krita-mode))

;; Org-Ehtml
(leaf org-ehtml
  :setq
  (org-ehtml-docroot . '(expand-file-name "~/org/roam"))
  (org-ehtml-everything-editable . t))

(require 'org-ehtml)

(defun start-ehtml ()
  (interactive)
  (ws-start org-ethml-handler 8888))

;; Org-Journal

;; Org-Roam
(leaf org-roam
  :custom
  (org-roam-directory . "~/org/roam")
  (org-roam-completion-everywhere . t)
  (org-roam-completion-system . 'default)
  (org-roam-dailies-directory . "~/org/roam/daily")
  :bind (("C-c n l" . org-roam-buffer-toggle)
		 ("C-c n f" . org-roam-node-find)
		 ("C-c n g" . org-roam-graph)
		 ("C-c n i" . org-roam-node-insert)
		 ("C-c n c" . org-roam-capture)
		 ;; Dailies
		 ("C-c n j" . org-roam-dailies-capture-today))
  
  :config
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  (require 'org-roam-protocol))

;; Disable Warning About Org-Roamv2
(setq org-roam-v2-ack t)

(leaf org-roam-ui
  :config
  (setq org-roam-ui-sync-theme t
		org-roam-ui-follow t
		org-roam-ui-update-on-save t
		org-roam-ui-open-on-start t))

;; Org Roam UI
;; Commented due to the fact that I have not yet installed straigt.el
;; (use-package org-roam-ui
;;   :straight
;;     (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
;;     :after org-roam
;; ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;; ;;         a hookable mode anymore, you're advised to pick something yourself
;; ;;         if you don't care about startup time, use
;; ;;  :hook (after-init . org-roam-ui-mode)
;;     :config
;;     (setq org-roam-ui-sync-theme t
;;           org-roam-ui-follow t
;;           org-roam-ui-update-on-save t
;;           org-roam-ui-open-on-start t))

;; Org-Bullets
(leaf org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;; Org-Starter
(leaf org-starter)

(leaf org-context)
(leaf org-hyperscheduler)
(leaf org-custom-cookies)
(leaf org-wild-notifier)
(leaf org-transclusion)
(leaf org-time-budgets)
(leaf org-ref-prettify)
(leaf org-drill-table)
(leaf org-auto-tangle)
(leaf org-tree-slide)
(leaf org-tanglesync)
(leaf org-randomnote)
(leaf org-projectile)
(leaf org-inline-pdf)
(leaf citar-org-roam)
(leaf org-web-tools)
(leaf org-treeusage)
(leaf org-rich-yank)
(leaf org-review)
(leaf org-re-reveal)
(leaf org-pdftools)
(leaf org-mind-map)
(leaf org-make-toc)
(leaf org-download)
(leaf org-contacts)
(leaf org-tagged
  :emacs>= 28.1)
(leaf org-kanban)
(leaf org-appear)
(leaf org-emms)
(leaf org-evil)
(leaf org-edna)
(leaf org-ref)
(leaf org-msg)
(leaf org-gtd)
(leaf org-wc)
(leaf org-if)
(leaf org-fancy-priorities)

(provide 'k-org)
