;; -*- lexical-binding: t -*-

(leaf transient
  :elpaca t)

(leaf cond-let
  :elpaca (cond-let :host github :repo "tarsius/cond-let")
  :require t)

(leaf magit
  :elpaca t
  :require t
  :elpaca (magit :branch "main" :pre-build ("make" "info"))
  :bind
  ("C-x g" . 'magit-status)
  :custom
  (magit-display-buffer-function . 'magit-display-buffer-same-window-except-diff-v1)
  :hook ((magit-mode . magit-delta-mode)
		 (magit-log-mode-hook . display-line-numbers-mode)
		 (magit-log-mode-hook . unpackaged/magit-log-date-headers-mode))
  :config
  (require 'magit-extras)
  (require 'magit-section)
  (defun unpackaged/magit-log--add-date-headers (&rest _ignore)
	"Add date headers to Magit log buffers."
	(when (derived-mode-p 'magit-log-mode)
	  (save-excursion
		(ov-clear 'date-header t)
		(goto-char (point-min))
		(cl-loop with last-age
				 for this-age = (-some--> (ov-in 'before-string 'any (line-beginning-position) (line-end-position))
								  car
								  (overlay-get it 'before-string)
								  (get-text-property 0 'display it)
								  cadr
								  (s-match (rx (group (1+ digit) ; number
													  " "
													  (1+ (not blank))) ; unit
											   (1+ blank) eos)
										   it)
								  cadr)
				 do (when (and this-age
							   (not (equal this-age last-age)))
					  (ov (line-beginning-position) (line-beginning-position)
						  'after-string (propertize (concat " " this-age "\n")
													'face 'magit-section-heading)
						  'date-header t)
					  (setq last-age this-age))
				 do (forward-line 1)
				 until (eobp)))))

  (define-minor-mode unpackaged/magit-log-date-headers-mode
	"Display date/time headers in `magit-log' buffers."
	:global t
	(if unpackaged/magit-log-date-headers-mode
		(progn
		  ;; Enable mode
		  (add-hook 'magit-post-refresh-hook #'unpackaged/magit-log--add-date-headers)
		  (advice-add #'magit-setup-buffer-internal :after #'unpackaged/magit-log--add-date-headers))
	  ;; Disable mode
	  (remove-hook 'magit-post-refresh-hook #'unpackaged/magit-log--add-date-headers)
	  (advice-remove #'magit-setup-buffer-internal #'unpackaged/magit-log--add-date-headers)))

  ;; Add Pull Requests
  (defun k/add-PR-fetch-ref (&optional remote-name)
	"If refs/pull is not defined on a GH repo, define it.

If REMOTE-NAME is not specified, it defaults to the `remote' set
for the \"main\" or \"master\" branch."
	(let* ((remote-name (or remote-name
							(magit-get "branch" "main" "remote")
							(magit-get "branch" "master" "remote")))
		   (remote-url (magit-get "remote" remote-name "url"))
		   (fetch-refs (and (stringp remote-url)
							(string-match "github" remote-url)
							(magit-get-all "remote" remote-name "fetch")))
		   ;; https://oremacs.com/2015/03/11/git-tricks/
		   (fetch-address (format "+refs/pull/*/head:refs/pull/%s/*" remote-name)))
	  (when fetch-refs
		(unless (member fetch-address fetch-refs)
		  (magit-git-string "config"
							"--add"
							(format "remote.%s.fetch" remote-name)
							fetch-address)))))
  (add-hook 'magit-mode-hook #'k/add-PR-fetch-ref))

(leaf magit-section
  :after (magit)
  :elpaca t)

(leaf magit-gh-pulls
  :doc "Creator of magit-gh-pulls now uses forge instead"
  :disabled t
  :elpaca t
  :require t
  :hook (magit-mode-hook . turn-on-magit-gh-pulls))

(leaf magit-prime
  :elpaca (magit-prime :host github :repo "Azkae/magit-prime")
  :hook (magit-pre-refresh-hook . magit-prime-refresh-cache))

(leaf magit-delta
  :after magit
  :elpaca t)

(leaf magit-todos
  :after (magit)
  :elpaca t
  :require t)

(leaf magit-org-todos
  :after (magit)
  :elpaca t
  :config
  (magit-org-todos-autoinsert))

(leaf magit-file-icons
  :disabled t
  :after (magit)
  :elpaca t
  :config
  (magit-file-icons-mode 1)
  :custom
  ;; These are the default values:
  (magit-file-icons-enable-diff-file-section-icons . t)
  (magit-file-icons-enable-untracked-icons . t)
  (magit-file-icons-enable-diffstat-icons . t))

(leaf pretty-magit
  :doc "idk broken"
  :disabled t
  :after (magit)
  :config
  (defmacro pretty-magit (WORD ICON PROPS &optional NO-PROMPT?)
	"Replace sanitized WORD with ICON, PROPS and by default add to prompts."
	`(prog1
		 (add-to-list 'pretty-magit-alist
					  (list (rx bow (group ,WORD (eval (if ,NO-PROMPT? "" ":"))))
							,ICON ',PROPS))
       (unless ,NO-PROMPT?
		 (add-to-list 'pretty-magit-prompt (concat ,WORD ": ")))))

  (setq pretty-magit-alist nil)
  (setq pretty-magit-prompt nil)
  ;; (pretty-magit "Feature" ? (:foreground "slate gray" :height 1.2))
  (pretty-magit "feat" ? (:foreground "slate gray" :height 1.2))
  ;; (pretty-magit "Add"     ? (:foreground "#375E97" :height 1.2))
  (pretty-magit "add"     ? (:foreground "#375E97" :height 1.2))
  ;; (pretty-magit "Fix"     ? (:foreground "#FB6542" :height 1.2))
  (pretty-magit "fix"     ? (:foreground "#FB6542" :height 1.2))
  ;; (pretty-magit "Clean"   ? (:foreground "#FFBB00" :height 1.2))
  (pretty-magit "clean"   ? (:foreground "#FFBB00" :height 1.2))
  ;; (pretty-magit "Docs"    ? (:foreground "#3F681C" :height 1.2))
  (pretty-magit "docs"    ? (:foreground "#3F681C" :height 1.2))
  ;; (pretty-magit "Build"    ? (:foreground "#3F681C" :height 1.2))
  (pretty-magit "build"    ? (:foreground "#3F681C" :height 1.2))
  ;; (pretty-magit "Chore"    ? (:foreground "#3F681C" :height 1.2))
  (pretty-magit "chore"    ? (:foreground "#3F681C" :height 1.2))
  ;; (pretty-magit "Style"    ? (:foreground "#3F681C" :height 1.2))
  (pretty-magit "style"    ? (:foreground "#3F681C" :height 1.2))
  ;; (pretty-magit "CI"    ? (:foreground "#3F681C" :height 1.2))
  (pretty-magit "ci"    ? (:foreground "#3F681C" :height 1.2))
  ;; (pretty-magit "Refactor"    ? (:foreground "#3F681C" :height 1.2))
  (pretty-magit "refactor"    ? (:foreground "#3F681C" :height 1.2))
  ;; (pretty-magit "Perfomance"    ? (:foreground "#3F681C" :height 1.2))
  (pretty-magit "perf"    ? (:foreground "#3F681C" :height 1.2))
  ;; (pretty-magit "Test"    ? (:foreground "#3F681C" :height 1.2))
  (pretty-magit "test"    ? (:foreground "#3F681C" :height 1.2))
  ;; (pretty-magit "Revert"    ? (:foreground "#3F681C" :height 1.2))
  (pretty-magit "revert"    ? (:foreground "#3F681C" :height 1.2))
  (pretty-magit "master"  ? (:box t :height 1.2) t)
  (pretty-magit "origin"  ?󰭃 (:box t :height 1.2) t)

  (defun add-magit-faces ()
	"Add face properties and compose symbols for buffer from pretty-magit."
	(interactive)
	(with-silent-modifications
      (--each pretty-magit-alist
		(-let (((rgx icon props) it))
          (save-excursion
			(goto-char (point-min))
			(while (search-forward-regexp rgx nil t)
              (compose-region
               (match-beginning 1) (match-end 1) icon)
              (when props
				(add-face-text-property
				 (match-beginning 1) (match-end 1) props))))))))

  (advice-add 'magit-status :after 'add-magit-faces)
  (advice-add 'magit-refresh-buffer :after 'add-magit-faces)

  (setq use-magit-commit-prompt-p nil)
  (defun use-magit-commit-prompt (&rest args)
	(setq use-magit-commit-prompt-p t))

  (defun magit-commit-prompt ()
	"Magit prompt and insert commit header with faces."
	(interactive)
	(when use-magit-commit-prompt-p
      (setq use-magit-commit-prompt-p nil)
      (insert (completing-read "Commit Type: " pretty-magit-prompt
							   nil ;; require-match is t by default
							   t ;; require a match
							   nil ;; initial input
							   nil ;; history
							   "Add: ") ;; preselect
			  )
	  
	  (add-magit-faces)))

  (remove-hook 'git-commit-setup-hook 'with-editor-usage-message)
  (add-hook 'git-commit-setup-hook 'magit-commit-prompt)
  (advice-add 'magit-commit :after 'use-magit-commit-prompt))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (leaf orgit											  ;;
;;   :elpaca (orgit :host github :repo "magit/orgit")) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; TODO: Configure Forge
(leaf forge
  :after (magit)
  :elpaca t
  :config
  ;; hack to eliminate weirdness
  (unless (boundp 'bug-reference-auto-setup-functions)
	(defvar bug-reference-auto-setup-functions '())))

(leaf diff-hl
  :elpaca t
  :require t
  :init
  (global-diff-hl-mode 1)
  :hook ((magit-post-refresh-hook . diff-hl-magit-post-refresh))
  :bind (:diff-hl-mode-map
		 ("<left-fringe> <mouse-1>" . 'diff-hl-diff-goto-hunk)
		 ("M-C-]" . 'diff-hl-next-hunk)
		 ("M-C-[" . 'diff-hl-previous-hunk)))

(leaf git-gutter
  ;; no reason to use this
  :disabled t
  :elpaca t)

(leaf code-review
  :elpaca t
  :after (magit forge)
  :bind ((:forge-topic-mode-map ("C-c r" . #'code-review-forge-pr-at-point))
		 (:code-review-mode-map (("C-c n" . #'code-review-comment-jump-next)
								 ("C-c p" . #'code-review-comment-jump-previous)))))

(leaf blamer
  :elpaca t
  :config
  (global-blamer-mode 1))

(leaf syncthing
  ;; requires diffutils
  :config
  (defun my/resolve-orgzly-syncthing ()
	(interactive)
	(ibizaman/syncthing-resolve-conflicts "~/cloud/orgzly"))

  (defun ibizaman/syncthing-resolve-conflicts (directory)
	"Resolve all conflicts under given DIRECTORY."
	(interactive "D")
	(let* ((all (ibizaman/syncthing--get-sync-conflicts directory))
		   (chosen (ibizaman/syncthing--pick-a-conflict all)))
	  (ibizaman/syncthing-resolve-conflict chosen)))

  (defun ibizaman/syncthing-show-conflicts-dired (directory)
	"Open dired buffer at DIRECTORY showing all syncthing conflicts."
	(interactive "D")
	(find-name-dired directory "*.sync-conflict-*"))

  (defun ibizaman/syncthing-resolve-conflict-dired (&optional arg)
	"Resolve conflict of first marked file in dired or close to point with ARG."
	(interactive "P")
	(let ((chosen (car (dired-get-marked-files nil arg))))
	  (ibizaman/syncthing-resolve-conflict chosen)))

  (defun ibizaman/syncthing-resolve-conflict (conflict)
	"Resolve CONFLICT file using ediff."
	(let* ((normal (ibizaman/syncthing--get-normal-filename conflict)))
	  (ibizaman/ediff-files
	   (list conflict normal)
	   `(lambda ()
		  (when (y-or-n-p "Delete conflict file? ")
			(kill-buffer (get-file-buffer ,conflict))
			(delete-file ,conflict))))))

  (defun ibizaman/syncthing--get-sync-conflicts (directory)
	"Return a list of all sync conflict files in a DIRECTORY."
	(directory-files-recursively directory "\\.sync-conflict-"))

  (defvar ibizaman/syncthing--conflict-history nil
	"Completion conflict history")

  (defun ibizaman/syncthing--pick-a-conflict (conflicts)
	"Let user choose the next conflict from CONFLICTS to investigate."
	(completing-read "Choose the conflict to investigate: " conflicts
					 nil t nil ibizaman/syncthing--conflict-history))


  (defun ibizaman/syncthing--get-normal-filename (conflict)
	"Get non-conflict filename matching the given CONFLICT."
	(replace-regexp-in-string "\\.sync-conflict-.*\\(\\..*\\)$" "\\1" conflict))


  (defun ibizaman/ediff-files (&optional files quit-hook)
	(interactive)
	(lexical-let ((files (or files (dired-get-marked-files)))
				  (quit-hook quit-hook)
				  (wnd (current-window-configuration)))
	  (if (<= (length files) 2)
		  (let ((file1 (car files))
				(file2 (if (cdr files)
						   (cadr files)
						 (read-file-name
						  "file: "
						  (dired-dwim-target-directory)))))
			(if (file-newer-than-file-p file1 file2)
				(ediff-files file2 file1)
			  (ediff-files file1 file2))
			(add-hook 'ediff-after-quit-hook-internal
					  (lambda ()
						(setq ediff-after-quit-hook-internal nil)
						(when quit-hook (funcall quit-hook))
						(set-window-configuration wnd))))
		(error "no more than 2 files should be marked")))))

(leaf git-timemachine
  :elpaca t)

(leaf difftastic
  :elpaca (difftastic :host github :repo "pkryger/difftastic.el")
  :require t difftastic-bindings
  :config
  (difftastic-bindings-mode))

(provide 'k-vc)
