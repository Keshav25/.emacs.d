(leaf org
  :bind (("C-c l" . org-store-link)
		 ("C-c C-M-l" . org-toggle-link-display))
  :setq
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
  :custom
  (org-directory . "~/Documents/org")
  (org-file-apps . '((auto-mode . emacs)))
  :hook ((org-mode-hook . (lambda ()
							(setq-local electric-pair-inhibit-predicate
										`(lambda (c)
										   (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c)))))))
  :config
  ;; https://github.com/karthink/.emacs.d/blob/5c9bb4102e53a60a7f6df2d3fb1cad5086114d1b/lisp/setup-org.el#L172
  (defun my/org-element-descendant-of (type element)
	"Return non-nil if ELEMENT is a descendant of TYPE.
TYPE should be an element type, like `item' or `paragraph'.
ELEMENT should be a list like that returned by `org-element-context'."
	;; MAYBE: Use `org-element-lineage'.
	(when-let* ((parent (org-element-property :parent element)))
	  (or (eq type (car parent))
		  (my/org-element-descendant-of type parent))))

  (defun my/org-return-dwim (&optional default)
	"A helpful replacement for `org-return'.  With prefix, call `org-return'.
On headings, move point to position after entry content.  In
lists, insert a new item or end the list, with checkbox if
appropriate.  In tables, insert a new row or end the table."
	;; Inspired by John Kitchin:
	;; http://kitchingroup.cheme.cmu.edu/blog/2017/04/09/A-better-return-in-org-mode/
	(interactive "P")
	(if default
		(org-return)
	  (cond
	   ;; Act depending on context around point.
	   
	   ((and (eq 'link (car (org-element-context)))
			 org-return-follows-link)
		;; Link: Open it.
		(org-open-at-point-global))

	   ((org-at-heading-p)
		;; Heading: Move to position after entry content.
		;; NOTE: This is probably the most interesting feature of this function.
		(let ((heading-start (org-entry-beginning-position)))
		  (goto-char (org-entry-end-position))
		  (cond ((and (org-at-heading-p)
					  (= heading-start (org-entry-beginning-position)))
				 ;; Entry ends on its heading; add newline after
				 (end-of-line)
				 (insert "\n\n"))
				(t
				 ;; Entry ends after its heading; back up
				 (forward-line -1)
				 (end-of-line)
				 (when (org-at-heading-p)
				   ;; At the same heading
				   (forward-line)
				   (insert "\n")
				   (forward-line -1))
				 ;; FIXME: looking-back is supposed to be called with more arguments.
				 (while (not (looking-back (rx (repeat 3 (seq (optional blank) "\n")))))
				   (insert "\n"))
				 (forward-line -1)))))

	   ((org-in-item-p)
		;; Plain list.  Yes, this gets a little complicated...
		(let ((context (org-element-context)))
		  (if (or (eq 'plain-list (car context))  ; First item in list
				  (and (eq 'item (car context))
					   (not (eq (org-element-property :contents-begin context)
								(org-element-property :contents-end context))))
				  (my/org-element-descendant-of 'item context))  ; Element in list item, e.g. a link
			  ;; Non-empty item: Add new item.
			  (if (org-at-item-checkbox-p)
				  (org-insert-todo-heading nil)
				(org-insert-item))
			;; Empty item: Close the list.
			;; TODO: Do this with org functions rather than operating on the
			;; text. Can't seem to find the right function.
			(delete-region (line-beginning-position) (line-end-position))
			(insert "\n"))))

	   ((when (fboundp 'org-inlinetask-in-task-p)
		  (org-inlinetask-in-task-p))
		;; Inline task: Don't insert a new heading.
		(org-return))

	   ((org-at-table-p)
		(cond ((save-excursion
				 (beginning-of-line)
				 ;; See `org-table-next-field'.
				 (cl-loop with end = (line-end-position)
						  for cell = (org-element-table-cell-parser)
						  always (equal (org-element-property :contents-begin cell)
										(org-element-property :contents-end cell))
						  while (re-search-forward "|" end t)))
			   ;; Empty row: end the table.
			   (delete-region (line-beginning-position) (line-end-position))
			   (org-return))
			  (t
			   ;; Non-empty row: call `org-return'.
			   (org-return))))
	   (t
		;; All other cases: call `org-return'.
		(org-return)))))
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
  :custom
  (org-agenda-files . '("~/Documents/org/agenda/"))
  :hook ((org-agenda-finalize . org-modern-agenda)
		 (org-agenda-finalize . hl-line-mode)))

(leaf org-capture
  :bind (("C-c c" . org-capture))
  :custom
  (org-capture-templates
   .
   '(
	 ("p" "Protocol" entry (file+headline "inbox.org" "Inbox")
	  "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
	 ("L" "Protocol Link" entry (file+headline "inbox.org" "Inbox")
	  "* %? %:annotation\n")
	 ("i" "inbox" entry
	  (file "~/Documents/org/inbox.org")
	  "* %?\n %T\n %a\n %i\n"
	  :empty-lines 1 )
	 ("c" "compsol"
	  entry (file "~/Documents/org/compsol.org")
	  "* %?\n %T\n %i\n"
	  :empty-lines 1)
	 ("s" "school"
	  entry (file "~/Documents/org/school.org")
	  "* %?\n %T\n %i\n"
	  :empty-lines 1)
	 ("f" "food"
	  entry (file "~/Documents/org/food-journal.org")
	  "* %?\n %T\n %i\n"
	  :empty-lines 1)
	 ("d" "daily-template"
	  entry
	  (file+olp+datetree "daily.org")
	  "%[~/Documents/org/daily-template]"
	  ;; :unnarrowed 1
	  :tree-type week
	  ;; :time-prompt t
	  ))))

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
  :bind
  ("C-c C-j" . org-journal-new-entry))

;; Evil Org
(leaf evil-org
  :ensure t
  :after org
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

;; Org-Noter
(leaf org-noter :ensure t)
(leaf org-noter-pdftools :ensure t)

(leaf org-ehtml
  :require t
  :ensure t
  :setq
  (org-ehtml-docroot . '(expand-file-name "~/Documents/org/roam"))
  (org-ehtml-everything-editable . t))

(defun start-ehtml ()
  (interactive)
  (ws-start org-ethml-handler 8888))

;; Org-Roam
(leaf org-roam
  :ensure t
  :custom
  (org-roam-directory . "~/Documents/org/roam")
  (org-roam-completion-everywhere . t)
  (org-roam-completion-system . 'default)
  (org-roam-dailies-directory . "~/Documents/org/roam/daily")
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

(leaf org-re-reveal
  :ensure t
  :setq
  (org-re-reveal-root . "~/src/reveal.js")
  (org-re-reveal-subtree-with-title-slide . t)
  :config
  (add-to-list 'org-structure-template-alist '("R" . "#+REVEAL_HTML: ?\n")))


(leaf org-pdftools)
(leaf org-mind-map)
(leaf org-make-toc)

(leaf org-download
  :ensure t
  :hook ((dired-mode . org-download-enable)
		 (org-mode . org-download-enable)))

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

(leaf org-modern
  :ensure t
  :init
  (global-org-modern-mode))

(leaf org-present
  :ensure t)

(leaf cdlatex
  ;; :ensure t
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

(provide 'k-org)
