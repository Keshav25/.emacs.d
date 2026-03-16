;; -*- lexical-binding: t -*-

(leaf denote
  :elpaca (denote :host github :repo "protesilaos/denote")
  :require t
  :bind
  (("C-c n n" . denote)
   ("C-c n N" . denote-type)
   ("C-c n i" . denote-link-or-create)
   ("C-c n r" . denote-open-or-create)
   ("C-c n b" . denote-find-backlink)
   ("C-c n d" . denote-date)
   ("C-c n l" . denote-find-link)
   ("C-c n h" . denote-org-extras-link-to-heading)
   ("C-c n R" . denote-rename-file-using-front-matter)
   ("C-c n k" . denote-rename-file-keywords))
  :custom
  (denote-directory . "~/Documents/notes/")
  (denote-save-buffer-after-creation . nil)
  (denote-known-keywords . '("projects" "areas" "resources" "archives"
                              "emacs" "philosophy" "politics" "economics"
                              "astrology" "journal" "books"
                              "meeting" "idea" "reference" "review"
                              "elfeed" "programming"))
  (denote-infer-keywords . t)
  (denote-prompts . '(title keywords template))
  (denote-backlinks-show-context . t)
  (denote-org-capture-specifiers . "%?")
  (denote-date-prompt-use-org-read-date . t)
  :custom-face
  (denote-faces-link . '((t (:slant italic))))
  :hook
  (dired-mode-hook . denote-dired-mode)
  (after-save-hook . k/denote-always-rename-on-save)
  :config
  (denote-rename-buffer-mode 1)

  (defvar k/denote-para-keywords '("projects" "areas" "resources" "archives"))

  (defun k/denote-assign-para ()
    (interactive)
    (if-let* ((file (buffer-file-name))
              ((denote-filename-is-note-p file))
              (all-keywords (string-split (denote-retrieve-filename-keywords file) "_"))
              (keywords (seq-remove (lambda (kw)
                                      (member kw k/denote-para-keywords))
                                    all-keywords))
              (para (completing-read "PARA category: " k/denote-para-keywords nil t))
              (new-keywords (cons para keywords)))
        (denote-rename-file
         file
         (denote-retrieve-title-or-filename file (denote-filetype-heuristics file))
         new-keywords
         (denote-retrieve-filename-signature file))
      (message "Current buffer is not a Denote file.")))

  (defun k/denote-always-rename-on-save ()
    (let ((denote-rename-confirmations nil)
          (denote-save-buffers t))
      (when (and buffer-file-name (denote-file-is-note-p buffer-file-name))
        (ignore-errors (denote-rename-file-using-front-matter buffer-file-name)))))

  (setq denote-templates
        '((meeting . "#+filetags: :meeting:\n\n* Attendees\n- \n\n* Agenda\n- \n\n* Notes\n\n* Action Items\n- [ ] ")
          (book . "#+filetags: :books:\n\n* Summary\n\n* Key Ideas\n-\n\n* Quotes\n-\n\n* My Thoughts\n\n* Rating: /10")
          (project . "#+filetags: :projects:\n\n* Goal\n\n* Tasks\n- [ ] \n\n* Notes\n\n* Resources\n-\n\n* Status: Active")
          (idea . "#+filetags: :idea:\n\n* The Idea\n\n* Why It Matters\n\n* How To Explore\n- [ ] \n\n* Related Notes")
          (review . "#+filetags: :review:\n\n* What Went Well\n-\n\n* What Didn't\n-\n\n* Lessons Learned\n-\n\n* Next Steps\n- [ ] ")
          (reference . "#+filetags: :reference:\n\n* Source\n- URL: \n- Author: \n\n* Summary\n\n* Key Points\n-\n\n* My Notes")))

  (defun k/denote-from-elfeed ()
    "Create a note from the current elfeed entry."
    (interactive)
    (let* ((entry (cond
                   ((eq major-mode 'elfeed-show-mode) elfeed-show-entry)
                   ((eq major-mode 'elfeed-search-mode) (elfeed-search-selected :single))
                   (t (user-error "Not in an elfeed buffer"))))
           (title (elfeed-entry-title entry))
           (url (elfeed-entry-link entry))
           (feed (elfeed-entry-feed entry))
           (feed-title (when feed (or (elfeed-meta feed :title) (elfeed-feed-title feed))))
           (tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
           (keywords (append '("elfeed" "reference") tags)))
      (denote title keywords 'org nil nil nil)
      (goto-char (point-max))
      (insert (format "\n* Source\n- URL: %s\n- Feed: %s\n- Date: %s\n\n* Summary\n\n* Key Points\n-\n\n* Thoughts\n"
                      url (or feed-title "Unknown")
                      (format-time-string "%Y-%m-%d" (elfeed-entry-date entry))))))

  (defun k/denote-project-note ()
    "Create or open a note for the current project."
    (interactive)
    (let* ((project (project-current t))
           (project-root (project-root project))
           (project-name (file-name-nondirectory (directory-file-name project-root)))
           (existing (denote-directory-files
                      (format "_projects.*%s" (regexp-quote project-name)))))
      (if existing
          (find-file (car existing))
        (denote project-name '("projects" "programming") 'org nil nil 'project))))

  (defun k/denote-code-note ()
    "Create a note linked to the current code context."
    (interactive)
    (let* ((file (buffer-file-name))
           (line (line-number-at-pos))
           (func (which-function))
           (branch (when (fboundp 'magit-get-current-branch)
                     (magit-get-current-branch)))
           (project (ignore-errors (project-name (project-current))))
           (title (read-string "Note title: "
                               (format "%s: %s" (or project "code") (or func "")))))
      (denote title '("programming" "reference") 'org)
      (goto-char (point-max))
      (insert (format "\n* Code Context\n- File: =%s=\n- Line: %d\n- Function: =%s=\n- Branch: =%s=\n- Project: %s\n\n* Notes\n"
                      (or file "N/A") line (or func "N/A")
                      (or branch "N/A") (or project "N/A")))))

  (defun k/denote-commit-note ()
    "Create a note for documenting a significant commit or PR."
    (interactive)
    (let* ((branch (when (fboundp 'magit-get-current-branch)
                     (magit-get-current-branch)))
           (project (ignore-errors (project-name (project-current))))
           (title (read-string "Commit/PR note: "
                               (format "%s: %s" (or project "") (or branch "")))))
      (denote title '("programming" "reference") 'org)
      (goto-char (point-max))
      (insert (format "\n* Context\n- Project: %s\n- Branch: =%s=\n- Date: %s\n\n* What Changed\n-\n\n* Why\n-\n\n* Impact\n-\n"
                      (or project "N/A") (or branch "N/A")
                      (format-time-string "%Y-%m-%d")))))

  (defun k/denote-extract-to-note ()
    "Extract region into a new note and replace with a link."
    (interactive)
    (if (use-region-p)
        (let* ((text (buffer-substring-no-properties (region-beginning) (region-end)))
               (title (read-string "Title for extracted note: "))
               (keywords (denote-keywords-prompt)))
          (delete-region (region-beginning) (region-end))
          (save-excursion
            (denote title keywords 'org)
            (goto-char (point-max))
            (insert "\n" text "\n")
            (save-buffer))
          (denote-link denote-last-path nil nil))
      (message "No region selected")))

  (defun k/denote-grep ()
    (interactive)
    (let ((consult-ripgrep-args
           (concat consult-ripgrep-args " --type org")))
      (consult-ripgrep denote-directory)))

  (defun k/denote-stats ()
    (interactive)
    (let* ((all-files (denote-directory-files))
           (total (length all-files))
           (by-keyword (make-hash-table :test 'equal)))
      (dolist (file all-files)
        (let ((keywords (string-split
                         (or (denote-retrieve-filename-keywords file) "") "_")))
          (dolist (kw keywords)
            (unless (string-empty-p kw)
              (puthash kw (1+ (gethash kw by-keyword 0)) by-keyword)))))
      (let ((sorted-kws (sort (hash-table-keys by-keyword)
                              (lambda (a b) (> (gethash a by-keyword)
                                               (gethash b by-keyword))))))
        (message "Notes: %d | Top: %s"
                 total
                 (string-join
                  (cl-loop for kw in (seq-take sorted-kws 8)
                           collect (format "%s(%d)" kw (gethash kw by-keyword)))
                  ", ")))))

  (defun k/denote-daily-review ()
    "Journal + recent notes side-by-side."
    (interactive)
    (denote-journal-new-or-existing-entry)
    (split-window-right)
    (other-window 1)
    (let ((recent (seq-take (sort (denote-directory-files)
                                  (lambda (a b)
                                    (time-less-p
                                     (file-attribute-modification-time (file-attributes b))
                                     (file-attribute-modification-time (file-attributes a)))))
                            10)))
      (with-current-buffer (get-buffer-create "*Denote: Recent*")
        (read-only-mode -1)
        (erase-buffer)
        (insert "#+title: Recent Notes\n\n")
        (dolist (file recent)
          (let ((title (or (denote-retrieve-title-value file 'org) (file-name-base file)))
                (kws (denote-retrieve-filename-keywords file)))
            (insert (format "- [[denote:%s][%s]] (%s)\n"
                            (denote-retrieve-filename-identifier file)
                            title (or kws "")))))
        (org-mode)
        (goto-char (point-min))
        (read-only-mode 1))
      (switch-to-buffer "*Denote: Recent*")))

  (consult-denote-mode))

(leaf denote-journal
  :elpaca (denote-journal :host github :repo "protesilaos/denote-journal")
  :after denote
  :require t
  :bind (("C-c n j" . denote-journal-new-or-existing-entry)
         ("C-c n J" . k/denote-daily-review))
  :custom
  (denote-journal-extras-title-format . 'day-date-month-year))

(leaf org-capture
  :bind (("C-c c" . org-capture))
  :custom
  (org-capture-templates .
                         '(("n" "New note (Denote)" plain
                            (file denote-last-path)
                            #'denote-org-capture
                            :no-save t
                            :immediate-finish nil
                            :kill-buffer t
                            :jump-to-captured t)
                           ("f" "Fleeting note" plain
                            (file denote-last-path)
                            #'denote-org-capture
                            :no-save t
                            :immediate-finish nil
                            :kill-buffer t
                            :jump-to-captured t)
                           ("b" "Book note" plain
                            (file denote-last-path)
                            #'denote-org-capture
                            :no-save t
                            :immediate-finish nil
                            :kill-buffer t
                            :jump-to-captured t)
                           ("m" "Meeting note" plain
                            (file denote-last-path)
                            #'denote-org-capture
                            :no-save t
                            :immediate-finish nil
                            :kill-buffer t
                            :jump-to-captured t)
                           ("i" "Idea" plain
                            (file denote-last-path)
                            #'denote-org-capture
                            :no-save t
                            :immediate-finish nil
                            :kill-buffer t
                            :jump-to-captured t)
                           ("a" "Appointment" entry
                            (file+olp denote-journal-capture-entry-for-date "Appointments")
                            "* %(denote-journal-capture-timestamp) %^{Subject?}")
                           ("l" "Log entry" entry
                            (file "~/Documents/notes/log.org")
                            "* %U %?\n%i"
                            :empty-lines 1))))

(leaf denote-org
  :elpaca (denote-org :host github :repo "protesilaos/denote-org")
  :after denote)

(leaf denote-project-notes
  :elpaca t
  :require t)

(leaf denote-menu
  :after (denote)
  :elpaca t
  :bind ("C-c n m" . denote-menu-list-notes))

(leaf denote-refs
  :after (denote)
  :elpaca t)

(leaf denote-search
  :elpaca t
  :bind ("C-c n g" . denote-search))

(leaf consult-denote
  :after (denote consult)
  :elpaca t
  :bind (("C-c n f" . consult-denote-find)
         ("C-c n s" . consult-denote-grep)))

(leaf consult-notes
  :after (consult denote)
  :elpaca t
  :bind
  ("C-c n F" . consult-notes)
  ("C-c n S" . consult-notes-search-in-all-notes)
  :config
  (consult-notes-denote-mode t))

(leaf citar-denote
  :after (denote citar)
  :elpaca t
  :custom
  (citar-open-always-create-notes . t)
  :init
  (citar-denote-mode t))

(leaf howm
  :elpaca t
  :require howm-org t
  :config
  (setopt howm-file-name-format "%Y%m%dT%H%M%S.org")
  (setopt howm-view-title-header "#+title:")
  (setopt howm-directory denote-directory)

  (defun k/howm-basename-chop (str)
    (let ((dashes-pos (string-match "--" str)))
      (if dashes-pos (substring str 0 dashes-pos) str)))
  (advice-add 'howm-view-item-basename :filter-return #'k/howm-basename-chop)

  (defvar howm-view-title-regexp)
  (defun k/howm-cut-title (str)
    (let ((begin (when (string-match howm-view-title-regexp str)
                   (match-beginning 2))))
      (if begin (substring str begin) str)))
  (advice-add 'howm-view-item-summary :filter-return #'k/howm-cut-title))

(leaf howm-graph
  :elpaca (howm-graph-view :host github :repo "SenkiReign/howm-graph")
  :custom
  (howm-graph-notes-directory . "~/Documents/notes"))

(leaf calendar
  :after denote
  :custom
  (diary-file . "~/Documents/notes/diary"))

(leaf denote-agenda
  :after denote-journal
  :elpaca t
  :require t
  :config
  (setq denote-agenda-include-journal t)
  (setq denote-agenda-include-regexp "_projects\\|_areas")
  (denote-agenda-insinuate))

(provide 'k-denote)
