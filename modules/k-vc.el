(leaf magit
  :bind
  ("C-x g" . 'magit-status))

(leaf magit-delta
  :after magit
  :ensure t)

(leaf forge
  :ensure t)

(leaf diff-hl
  :ensure t)

(leaf git-gutter
  :ensure t)

;; (leaf code-review
  ;; :ensure t)

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

(provide 'k-vc)
