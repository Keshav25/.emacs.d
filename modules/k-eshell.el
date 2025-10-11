;; -*- lexical-binding: t -*-

(leaf eshell
  :require t
  :require 'em-smart 'em-alias
  :custom
  (eshell-banner-message . "===Eshell===\n")
  (eshell-where-to-jump . 'begin)
  (eshell-review-quick-commands . nil)
  (eshell-smart-space-goes-to-end . t)
  (eshell-delchar-or-maybe-eof . t)
  (eshell-modules-list . '(eshell-banner eshell-basic eshell-cmpl eshell-dirs
										 eshell-extpipe eshell-glob eshell-hist eshell-ls
										 eshell-pred eshell-prompt eshell-script eshell-term
										 eshell-smart eshell-unix eshell-rebind))
  (eshell-history-size . 1024)
  (eshell-hist-ignoredups . t)
  (eshell-input-filter . 'k/eshell-input-filter)
  (eshell-list-files-after-cd . t)
  (eshell-pushd-dunique . t)
  :hook
  (eshell-directory-change . k/sync-dir-in-buffer-name)
  (eshell-mode . k/eshell-specific-outline-regepxp)
  (eshell-mode . (lambda ()
				   (setq-local completion-at-point-functions
							   '(pcomplete-completions-at-point cape-file cape-history))))
  :config
  (defun k/adviced-eshell-add-input-to-history (orig-fun &rest r)
    "Cd to relative paths aren't that useful in history. Change to absolute paths."
    (require 'seq)
    (let* ((input (nth 0 r))
           (args (progn
                   (set-text-properties 0 (length input) nil input)
                   (split-string input))))
      (if (and (equal "cd" (nth 0 args))
               (not (seq-find (lambda (item)
                                ;; Don't rewrite "cd /ssh:" in history.
                                (string-prefix-p "/ssh:" item))
                              args))
               (not (seq-find (lambda (item)
                                ;; Don't rewrite "cd -" in history.
                                (string-equal "-" item))
                              args)))
          (apply orig-fun (list (format "cd %s"
                                        (expand-file-name (concat default-directory
                                                                  (nth 1 args))))))
        (apply orig-fun r))))

  (advice-add #'eshell-add-input-to-history
			  :around
			  #'k/adviced-eshell-add-input-to-history)

  (defun eshell-new ()
	"Open a new instance of eshell."
	(interactive)
	(eshell 'N))

  (defun k/eshell-input-filter (input)
	(and
	 (eshell-input-filter-default input)
	 (eshell-input-filter-initial-space input)
	 (not (string-prefix-p "cd " input))
	 (not (string-prefix-p "ls " input))
	 (not (string-prefix-p "l  " input))))

  (defun k/sync-dir-in-buffer-name ()
	(let* ((root (project-root))
		   (root-name (project-name root)))
	  (if root-name
		  (rename-buffer (format "eshell %s/%s" root-name (s-chop-prefix root default-directory)) t)
		(rename-buffer (format "eshell %s" default-directory) t))))

  (defun k/eshell-redirect-to-buffer (buffer)
	(interactive (list (read-buffer "Redirect to buffer: ")))
	(insert (format " >>> #<%s>" buffer)))

  (defun k/eshell-specific-outline-regexp ()
	(setq-local outline-regexp eshell-prompt-regexp))

  :bind
  ("C-c o e" . eshell-new)
  (:eshell-command-mode-map
   ("C-c >" . k/eshell-redirect-to-buffer)))


(leaf run-in-eshell
  :init
  (defun run-this-in-eshell (cmd)
	"Runs the command CMD in Eshell."
	(let ((eshell-buffer (current-buffer)))  ; Store the current buffer
      (with-current-buffer eshell-buffer
		(end-of-buffer)
		(eshell-kill-input)
		(message (concat "Running in Eshell: " cmd))
		(insert cmd)
		(eshell-send-input)
		(end-of-buffer)
		(eshell-bol)
		(yank)))))

(use-package clear-eshell
  :init
  (defun eshell/myclear ()
	(interactive)
	(run-this-in-eshell "clear 1"))
  :bind ((:map eshell-command-map
			   ("C-c C-l" . eshell/myclear))))

;; '(eshell-alias eshell-banner eshell-basic eshell-cmpl eshell-dirs
;; 			   eshell-extpipe eshell-glob eshell-hist eshell-ls
;; 			   eshell-pred eshell-prompt eshell-script eshell-term
;; 			   eshell-unix)

(leaf eshell-icat
  :after eshell
  :config
  ;; Eshell mycat command
  ;; To know if I need to print as image or I need to default to the default eshell/cat
  (defun my-is-imagep (filename)
	(let ((extension (file-name-extension filename))
		  (image-extensions '("png" "jpg" "bmp")))
	  (member extension image-extensions)))

  ;; Creates a space with display properties. Feel free to change `eshell/println` to `insert` and use it in a normal emacs buffer, it will inline the path given in `file`.
  (defun my-print-image-eshell (file)
	(eshell/printnl (propertize " " 'display (create-image (expand-file-name file)))))


  ;; If image, use `my-print-image-eshell`. Otherwise, just use `eshell/cat`.
  (defun eshell/mycat (&rest args)
	(interactive)
	(mapc (lambda (arg)
			(if (my-is-imagep arg)
				(my-print-image-eshell arg)
			  (eshell/cat-with-syntax-highlighting arg)))
		  (-flatten args))
	nil)

  (defun eshell/cat-with-syntax-highlighting (filename)
	(let ((existing-buffer (get-file-buffer filename))
		  (buffer (find-file-noselect filename)))
	  (eshell-print
	   (with-current-buffer buffer
		 (if (fboundp 'font-lock-ensure)
			 (font-lock-ensure)
		   (with-no-warnings
			 (font-lock-fontify-buffer)))
		 (let ((contents (buffer-string)))
		   (remove-text-properties 0 (length contents) '(read-only nil) contents)
		   contents)))
	  (unless existing-buffer
		(kill-buffer buffer))
	  nil))

  (advice-add 'eshell/cat :override #'eshell/mycat))

(leaf esh-help
  :after eshell
  :elpaca t
  :require t
  :config
  (setup-esh-help-eldoc))

(leaf virtualenvwrapper
  :elpaca t)

(leaf eshell-prompt-extras
  :after (virtualenvwrapper)
  :elpaca t
  :config
  (require 'virtualenvwrapper)
  (venv-initialize-eshell)
  (autoload 'epe-theme-lambda "eshell-prompt-extras")
  (setopt eshell-highlight-prompt nil
		  eshell-prompt-function 'epe-theme-lambda))

(leaf eshell-z
  :after eshell
  :elpaca t
  :require t)

(leaf eshell-vterm
  :after (eshell vterm)
  :elpaca t
  :config
  (eshell-vterm-mode 1)
  (defalias 'eshell/v 'eshell-exec-visual))

(leaf eshell-bookmark
  :after eshell
  :elpaca t
  :require t
  :hook (eshell-mode . eshell-bookmark-setup))

(leaf eshell-did-you-mean
  :after eshell
  :elpaca t
  :config
  (eshell-did-you-mean-setup))

(leaf eshell-syntax-highlighting
  :after eshell
  :elpaca t
  :hook
  (eshell-first-time-mode . eshell-syntax-highlighting-global-mode)
  :init
  (defface eshell-syntax-highlighting-invalid-face
	'((t :inherit diff-error))
	"Face used for invalid Eshell commands."
	:group 'eshell-syntax-highlighting))

(leaf eat
  :after eshell
  :elpaca t
  :bind (:eat-semi-char-mode-map
		 ("M-t" . execute-extended-command)
		 ("M-x" . transpose-words))
  :custom
  (eat-shell . "~/.guix-profile/bin/nu")
  :hook ((eshell-load-hook . eat-eshell-mode)
		 ;; since htop doesn't work with the former
		 (eshell-load-hook . eat-eshell-visual-command-mode)))

(leaf comint-mime
  :elpaca t
  :hook (shell-mode-hook . comint-mime-setup)
  (inferior-python-code-mode . comint-mime-setup)
  :config
  (when (executable-find "ipython3")
	(setq python-shell-interpreter "ipython3"
          python-shell-interpreter-args "--simple-prompt --classic")))

(leaf eshell-git-prompt
  :after eshell
  :ensure t)

(provide 'k-eshell)
