(leaf org
  :ensure t
  :bind (("C-c l" . org-store-link)
		 ("C-c C-M-l" . org-toggle-link-display))
  :hook (org-mode . (turn-on-visual-line-mode))
  :custom
  (org-ellipsis . " ▾")
  (org-startup-numerated . 1)
  (org-startup-truncated . 1)
  (org-startup-indented . 1)
  (org-appear-mode . 1)
  (org-use-speed-commands . 1)
  (org-special-ctrl-a . 1)
  (org-special-ctrl-k . 1)
  (org-fontify-done-headline . t)
  (org-fontify-quote-and-verse-blocks . t)
  (org-fontify-whole-heading-line . t)
  (org-hidden-keywords . nil)
  (org-hide-emphasis-markers . t)
  (org-hide-leading-stars . t)
  (org-export-with-toc . nil)
  (org-agenda-search-view-always-boolean . t)
  (org-agenda-timegrid-use-ampm . nil)
  (org-return-follows-link . t)
  (org-mouse-1-follows-link . t)
  (org-refile-use-outline-path . 'file)
  ;; (org-agenda-time-grid . ((daily today require-timed)
  ;; (600 630 700 730 800 830 900 930 1000 1030 1100 1130 1200 1230 1300 1330 1400 1430 1500 1530 1600 1630 1700 1730 1800 1830 2000 2030 2100 2130 2200 2230 2300 2330)
  ;; " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
  (org-log-done . 'time)
  (org-log-into-drawer . t)
  ;; (org-todo-keywords . ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
  ;; (sequence "BACKLOG(b)" "ACTIVE(a)"
  ;; "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)"
  ;; "|" "DELEGATED(D)" "CANCELLED(c)")))
  
  (org-highlight-latex-and-related '(native))
  (org-use-sub-superscripts . '{})
  (org-directory . "~/Documents/org")
  (org-file-apps . '((auto-mode . emacs)))
  (org-src-ask-before-returning-to-edit-buffer . nil)
  (org-src-window-setup . 'current-window)
  :init
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
	 (emacs-lisp . t)
	 (shell . t)
	 (scheme . t))
   ))

(leaf org-agenda
  :bind (("C-c a" . org-agenda))
  :config
  (setq org-agenda-files (directory-files org-directory nil "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)"))
  (setq org-cycle-separator-lines 1)
  :custom
  (org-refile-targets . '((org-agenda-files :maxlevel . 3)))
  (org-agenda-window-setup . 'current-window)
  :hook ((org-agenda-finalize . org-modern-agenda)
		 (org-agenda-finalize . hl-line-mode)))

(leaf org-publish
  :custom
  (org-publish-project-alist .
							 '(("journal"
								:base-directory "~/Documents/journal/"
								:base-extension "org"
								:publishing-directory "~/org/neocities/journal"
								:recursive t
								:publishing-function org-html-publish-to-html
								:headline-levels 4
								:section-numbers nil
								:html-head nil
								:html-head-include-default-style nil
								:html-head-include-scripts nil
								:html-preamble my-blog-header
								:html-postamble my-blog-footer)
							   ("posts"
								:base-directory "~/Documents/posts/"
								:base-extension "org"
								:publishing-directory "~/org/neocities/posts"
								:recursive t
								:publishing-function org-html-publish-to-html
								:headline-levels 4
								:section-numbers nil
								:html-head nil
								:html-head-include-default-style nil
								:html-head-include-scripts nil
								:html-preamble my-blog-header
								:html-postamble my-blog-footer)
							   ("roam"
								:base-directory "~/org/roam/"
								:base-extension "org"
								:publishing-directory "~/org/zettelkasten/"
								:recursive t
								:publishing-function org-html-publish-to-html
								:headline-levels 4
								:section-numbers nil
								:html-head nil
								:html-head-include-default-style nil
								:html-head-include-scripts nil
								:html-preamble my-blog-header
								:html-postamble my-blog-footer)
							   )))

(leaf org-capture
  :bind (("C-c c" . org-capture))
  :custom
  (org-capture-templates .
						 '(("n" "New note with Denote" plain
							(file denote-last-path)
							#'denote-org-capture
							:no-save t
							:immediate-finish nil
							:kill-buffer t
							:jump-to-captured t)
						   ("f" "fleeting notes" plain
							(file denote-last-path)
							#'denote-org-capture
							:no-save t
							:immediate-finish nil
							:kill-buffer t
							:jump-to-captured t))))

;; :custom
;; (org-capture-templates
;;  .
;;  '(
;;    ("L" "Protocol Link" entry (file+headline "inbox.org" "Inbox")
;; 	"* %? %:annotation\n")
;;    ("i" "inbox" entry
;; 	(file "~/Documents/org/inbox.org")
;; 	"* %?\n %T\n %a\n %i\n"
;; 	:empty-lines 1 )
;;    ("e" "emacs"
;; 	entry (file "~/Documents/org/emacs-notes.org")
;; 	"* %?\n %T\n %i\n"
;; 	:empty-lines 1)
;;    ("c" "compsol"
;; 	entry (file "~/Documents/org/compsol.org")
;; 	"* %?\n %T\n %i\n"
;; 	:empty-lines 1)
;;    ("s" "school"
;; 	entry (file "~/Documents/org/school.org")
;; 	"* %?\n %T\n %i\n"
;; 	:empty-lines 1)
;;    ("f" "food"
;; 	entry (file "~/Documents/org/food-journal.org")
;; 	"* %?\n %T\n %i\n"
;; 	:empty-lines 1)
;;    ("d" "daily-template"
;; 	entry
;; 	(file+olp+datetree "daily.org")
;; 	"%[~/Documents/org/daily-template]"
;; 	;; :unnarrowed 1
;; 	:tree-type week
;; 	;; :time-prompt t
;; 	))))

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
  :ensure t
  :custom
  (org-journal-file-format . "%Y%m%d.org")
  :bind
  ("C-c C-j" . org-journal-new-entry))

;; Evil Org
(leaf evil-org
  :ensure t
  :after (evil org)
  :hook (org-mode . (lambda () (evil-org-mode)))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  :setq (evil-want-C-i-jump . nil))

;; PDF Tools
(leaf pdf-tools
  :ensure t
  :hook ((doc-view-mode-hook . (lambda () (require 'pdf-tools))))
  :config
  (pdf-tools-install)
  :setq-default
  (pdf-view-display-size . 'fit-width))

;; Calibre integration
(leaf calibredb
  :ensure t
  :config
  (setq calibredb-root-dir "~/Calibre Library")
  (setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
  (setq calibredb-library-alist '(
								  ("~/Calibre Library")
								  )))

;; read epub files
(leaf nov
  :ensure t)

;; https://depp.brause.cc/nov.el/
;; (use-package nov-xwidget
;;   :ensure t
;;   :demand t
;;   :after nov
;;   :config
;;   (define-key nov-mode-map (kbd "o") 'nov-xwidget-view)
;;   (add-hook 'nov-mode-hook 'nov-xwidget-inject-all-files))

;; Org-Noter
(leaf org-noter :ensure t)
(leaf org-noter-pdftools :ensure t)

(leaf org-ehtml
  :require t
  :ensure t
  :setq
  (org-ehtml-docroot . '(expand-file-name "~/org/roam"))
  (org-ehtml-everything-editable . t))

(defun start-ehtml ()
  (interactive)
  (ws-start org-ethml-handler 8888))

;; Org-Roam
(leaf org-roam
  :ensure t
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
		 ("C-c n d" . org-roam-dailies-capture-today))
  
  :config
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  (require 'org-roam-protocol))

;; Disable Warning About Org-Roamv2
(setq org-roam-v2-ack t)

(leaf org-roam-ui
  :ensure t
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

;; Org-Modern-Mode now
;; Org-Bullets
;; (leaf org-bullets
;; :after org
;; :hook (org-mode . org-bullets-mode)
;; :custom
;; (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;; Org-Starter
(leaf org-starter
  :ensure t)

(leaf org-context :ensure t)
(leaf org-hyperscheduler :ensure t)
(leaf org-custom-cookies :ensure t)
(leaf org-wild-notifier :ensure t)
(leaf org-transclusion :ensure t)
(leaf org-time-budgets :ensure t)
(leaf org-ref-prettify :ensure t)
(leaf org-drill-table :ensure t)
(leaf org-auto-tangle :ensure t)
(leaf org-tree-slide :ensure t)
(leaf org-tanglesync :ensure t)
(leaf org-randomnote :ensure t)
(leaf org-projectile :ensure t)
(leaf org-inline-pdf :ensure t)
(leaf citar-org-roam :ensure t)
(leaf org-web-tools :ensure t)
(leaf org-treeusage :ensure t)
(leaf org-rich-yank :ensure t)
(leaf org-review :ensure t)

(leaf org-re-reveal
  :ensure t
  :setq
  (org-re-reveal-root . "~/src/reveal.js")
  (org-re-reveal-subtree-with-title-slide . t)
  :config
  (add-to-list 'org-structure-template-alist '("R" . "#+REVEAL_HTML: ?\n")))


(leaf org-pdftools :ensure t)
(leaf org-mind-map :ensure t)
(leaf org-make-toc :ensure t)

(leaf org-download
  :ensure t
  :hook ((dired-mode . org-download-enable)
		 (org-mode . org-download-enable)))

(leaf org-contacts :ensure t)

(leaf org-tagged
  :ensure t
  :emacs>= 28.1)

(leaf org-kanban :ensure t)
(leaf org-appear :ensure t)
(leaf org-emms :ensure t)
(leaf org-evil :after evil :ensure t)
(leaf org-edna :ensure t)
(leaf org-ref :ensure t)
(leaf org-msg :ensure t)
(leaf org-gtd :ensure t)
(leaf org-wc :ensure t)
(leaf org-if :ensure t)
(leaf org-fancy-priorities :ensure t)

(leaf org-modern
  :ensure t
  :init
  (global-org-modern-mode))

(leaf org-present
  :ensure t)

(leaf cdlatex
  :ensure t
  :hook ((org-mode . turn-on-org-cd-latex)))

(leaf org-mime :ensure t)

(leaf writing-function
  :config
  (defun turn-on-writing ()
	(interactive)
	(display-line-numbers-mode 0)
	(visual-line-mode 1)
	(org-modern-mode 1))
  (defun turn-off-writing ()
	(interactive)
	(display-line-numbers-mode 1)
	(visual-line-mode 0)
	(org-modern-mode 0)))

(leaf pandoc :ensure t)
(leaf pandoc-mode :ensure t)
(leaf ox-pandoc :ensure t)
(leaf copyit-pandoc :ensure t)

(leaf gnuplot :ensure t)
(leaf htmlize :ensure t)

(leaf org-novelist
  :quelpa (org-novelist :fetcher github :repo "sympodius/org-novelist")
  :setq
  (org-novelist-language-tag ."en-US")
  (org-novelist-author . user-full-name)
  (org-novelist-author-email . user-mail-address))

(leaf ankifier
  :after (expand-region)
  :quelpa (ankifier :fetcher github :repo "adham-omran/ankifier")
  :setq
  (ankifier-insert-elsewhere . t)
  (ankifier-anki-basic-note-type . "E-Basic")
  (ankifier-anki-cloze-note-type . "E-Cloze")
  (ankifier-feedback . t)
  (ankifier-context-question . t))

(leaf memacs
  ;; this is a python program
  )

(leaf org-pretty-tags
  :ensure t
  :init
  (org-pretty-tags-global-mode 1))

(leaf org-unique-id
  :ensure t
  :require t
  :after (org)
  :hook ((before-save-hook . org-unique-id)))

(leaf ox-haunt
  :ensure t)

(leaf hyperbole
  :disabled t
  :ensure t
  :config
  (hyperbole-mode 1))

(leaf org-ql
  :quelpa (org-ql :fetcher github :repo "alphapapa/org-ql"
				  :files (:defaults (:exclude "helm-org-ql.el"))))

(leaf org-sticky-header
  :ensure t)

(leaf org-bookmark-heading
  :ensure t)

(leaf hammy
  :quelpa (hammy :fetcher github :repo "alphapapa/hammy.el"))

(leaf denote
  :ensure t
  :custom
  (denote-directory . "~/Documents/notes/")
  (denote-save-buffer-after-creation . nil)
  (denote-known-keywords . '("emacs"
							 "philosophy"
							 "politics"
							 "economics"
							 "astrology"))
  (denote-infer-keywords . t)
  (denote-prompts . '(title keywords template))
  (denote-templates . nil)
  (denote-backlinks-show-context . t)
  (denote-org-capture-specifiers . "%?"))

(leaf denote-menu
  :after (dmenu)
  :ensure t)

(leaf denote-refs
  :after (dmenu)
  :ensure t)

(leaf citar-denote
  :after (dmenu citar)
  :ensure t)

(leaf denote-explore
  :after (dmenu)
  :ensure t)

;; from https://www.reddit.com/r/emacs/comments/d54ogp/emacs_doom_e17_org_mode_checkboxes/
;; (add-hook 'org-mode-hook
;; 		  ;; TODO: Use add-to-list prettify-symbols-alist instead of add-to-list"", to avoid duplicates
;; 		  (lambda () ")Beautify Org Checkbox Symbol"
;;             ;; These are nice unicode characters for checkboxes: ☐ ☑ ☒
;;             (add-to-list prettify-symbols-alist '("TODO" . "☐") )
;;             (add-to-list prettify-symbols-alist '("NEXT" . "Δ" ) )
;;             (add-to-list prettify-symbols-alist '("DONE" . "☑" ) )
;;             (add-to-list prettify-symbols-alist '("CANCELED" . "☒" ) )

;;             ;; This changed to be case sensitive in a recent update of doom
;;             (add-to-list prettify-symbols-alist '("#+BEGIN_SRC" . "↦" ) )
;;             (add-to-list prettify-symbols-alist '("#+END_SRC" . "⇤" ) )
;;             (add-to-list prettify-symbols-alist '("#+BEGIN_EXAMPLE" . "↦" ) )
;;             (add-to-list prettify-symbols-alist '("#+END_EXAMPLE" . "⇤" ) )
;;             (add-to-list prettify-symbols-alist '("#+BEGIN_QUOTE" . "↦" ) )
;;             (add-to-list prettify-symbols-alist '("#+END_QUOTE" . "⇤" ) )

;;             (add-to-list prettify-symbols-alist '("#+begin_quote" . "↦" ) )
;;             (add-to-list prettify-symbols-alist '("#+end_quote" . "⇤" ) )
;;             (add-to-list prettify-symbols-alist '("#+begin_example" . "↦" ) )
;;             (add-to-list prettify-symbols-alist '("#+end_example" . "⇤" ) )
;;             (add-to-list prettify-symbols-alist '("#+begin_src" . "↦" ) )
;;             (add-to-list prettify-symbols-alist '("#+end_src" . "⇤" ) )

;;             ;; Monday 2021-11-01 not working
;;             ;; (add-to-list prettify-symbols-alist '("+ [ ]" . "☐") )
;;             ;; (add-to-list prettify-symbols-alist '("+ [x]" . "☑" ) )
;;             ;; (add-to-list prettify-symbols-alist '("+ []" . "☒" ) )

;;             (prettify-symbols-mode)))

(provide 'k-org)
