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
						   ("f" "Fleeting notes" plain
							(file denote-last-path)
							#'denote-org-capture
							:no-save t
							:immediate-finish nil
							:kill-buffer t
							:jump-to-captured t)
						   ("b" "Books" plain
							(file denote-last-path)
							#'denote-org-capture
							:no-save t
							:immediate-finish nil
							:kill-buffer t
							:jump-to-captured t)
						   ("a" "Appointment" entry
                            (file+olp denote-journal-capture-entry-for-date "Appointments")
                            "* %(denote-journal-capture-timestamp) %^{Subject?}"))))

(leaf denote
  :elpaca (denote :host github :repo "protesilaos/denote")
  :require t
  :config
  (denote-rename-buffer-mode 1)
  :bind
  ("C-c n i" . denote-link-or-create)
  ("C-c n c" . denote-open-or-create)
  ("C-c n j" . denote-journal-extras-new-or-existing-entry)
  ("C-c n b" . denote-find-backlink)
  ("C-c n d" . denote-date)
  ("C-c n l" . denote-find-link)
  ("C-c n h" . denote-org-extras-link-to-heading)
  :custom
  (denote-directory . "~/Documents/notes/")
  (denote-save-buffer-after-creation . nil)
  (denote-para-keywords . '("projects" "areas"
							"resources" "archives"))
  (denote-known-keywords . '(;; PARA
							 "projects"
							 "areas"
							 "resources"
							 "archives"
							 "emacs"
							 "philosophy"
							 "politics"
							 "economics"
							 "astrology"
							 "journal"
							 "books"))
  (denote-infer-keywords . t)
  (denote-prompts . '(title keywords template))
  (denote-templates . nil)
  (denote-backlinks-show-context . t)
  (denote-org-capture-specifiers . "%?")
  (denote-date-prompt-use-org-read-date . t)
  (denote-journal-extras-title-format . 'day-date-month-year)
  :custom-face
  (denote-faces-link . '((t (:slant italic))))
  :hook
  (dired-mode-hook . denote-dired-mode)
  (after-save-hook . k/denote-always-rename-on-save)
  :config
  (require 'denote-journal-extras)
  (require 'denote-org-extras)
  (defun k/publish-denote ()
	(interactive)
	(mapc (lambda (file) (org-ehtml-export-file file))
		  (seq-filter (apply-partially #'string-match-p "_programming")
					  (denote-directory-files)))
	(shell-command (concat "mv "
						   [O [I			   (concat (denote-directory) "*.html ")]]
						   (concat (denote-directory) "blog/posts/"))))
  (defun k/denote-always-rename-on-save ()
	"Rename the current Denote file upon saving the file.
    Add this to `after-save-hook'."
	(let ((denote-rename-confirmations nil)
          (denote-save-buffers t)) ; to save again post-rename
      (when (and buffer-file-name (denote-file-is-note-p buffer-file-name))
		(ignore-errors (denote-rename-file-using-front-matter buffer-file-name)))))
  (defun k-denote-assign-para ()
    (interactive)
	(if-let* ((file (buffer-file-name))
              ((denote-filename-is-note-p file))
              (all-keywords (string-split (denote-retrieve-filename-keywords file) "_"))
              (keywords (seq-remove (lambda (keyword)
                                      (member keyword denote-para-keywords))
									all-keywords))
              (para (completing-read "Select category: " denote-para-keywords))
              (new-keywords (push para keywords)))
		(denote-rename-file
		 file
		 (denote-retrieve-title-or-filename file (denote-filetype-heuristics file))
		 new-keywords
		 (denote-retrieve-filename-signature file))
      (message "Current buffer is not a Denote file.")))
  )

(leaf denote-agenda
  :after denote
  :elpaca t
  :require t
  :config
  (load-library "denote-journal-extras")
  (setq denote-agenda-include-journal t)
  (setq denote-agenda-include-regexp "")
  (denote-agenda-insinuate))

(leaf denote-journal
  :elpaca t)

(leaf denote-project-notes
  :elpaca t
  :require t)

(leaf denote-menu
  :after (denote)
  :elpaca t)

(leaf denote-refs
  :after (denote)
  :elpaca t)

(leaf citar-denote
  :after (denote citar)
  :elpaca t
  :custom
  (citar-open-always-create-notes . t)
  :init
  (citar-denote-mode t))

(leaf denote-explore
  :after (denote)
  :elpaca t)

(leaf consult-denote
  :after (denote consult)
  :elpaca t)

(leaf denote-search
  :elpaca t)


(leaf denote-tree
  :disabled t
  :after (denote)
  :elpaca (denote-tree :host "github.com" :repo "sarcom-sar/denote-tree.el"))

(leaf denote-say
  :disabled t
  :after (denote)
  :elpaca (denote-say :host "github.com" :repo "MirkoHernandez/denote-say.el"))

(leaf howm
  :elpaca t
  :require t
  :config
  (require 'howm-org)
  (setopt howm-file-name-format "%Y%m%dT%H%M%S.org")
  (setopt howm-view-title-header "#+title:")
  ;; Advise `howm-view-item-basename' so that if its return value
  ;; includes "--", only the text before is returned.
  (defun my/howm-basename-chop (str)
	"Advice for `howm-view-item-basename'.
Takes a file's basename, STR, and returns only the portion before
\"--\"."
	(let ((dashes-pos (string-match "--" str)))
      (cond (dashes-pos (substring str 0 dashes-pos))
			(t str))))

  (advice-add 'howm-view-item-basename :filter-return
              'my/howm-basename-chop)


  ;; Advise `howm-view-item-summary' so that it removes the "#+title: "
  ;; portion of note titles.
  (defvar howm-view-title-regexp)
  (defun my/howm-cut-title (str)
	"Remove `howm-view-title-header' plus whitespace from STR."
	(let ((begin (when (string-match howm-view-title-regexp str)
                   (match-beginning 2))))
      (if begin (substring str begin) str)))

  (advice-add 'howm-view-item-summary :filter-return 'my/howm-cut-title)
  (setopt howm-directory denote-directory)
  )

(provide 'k-denote)
