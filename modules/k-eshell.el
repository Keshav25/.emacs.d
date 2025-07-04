;; -*- lexical-binding: t -*-

(leaf eshell
  :require t
  :require 'em-smart 'em-alias
  :custom
  (eshell-where-to-jump . 'begin)
  (eshell-review-quick-commands . nil)
  (eshell-smart-space-goes-to-end . t)
  (eshell-delchar-or-maybe-eof . t)
  (eshell-modules-list . '(eshell-banner eshell-basic eshell-cmpl eshell-dirs
										 eshell-extpipe eshell-glob eshell-hist eshell-ls
										 eshell-pred eshell-prompt eshell-script eshell-term
										 eshell-smart eshell-unix eshell-rebind))
  :config
  (defun eshell-new ()
	"Open a new instance of eshell."
	(interactive)
	(eshell 'N))
  :bind
  ("C-c o e" . eshell-new))


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
	(eshell/printnl (propertize " " 'display (create-image file))))


  ;; If image, use `my-print-image-eshell`. Otherwise, just use `eshell/cat`.
  (defun eshell/mycat (&rest args)
	(interactive)
	(mapc (lambda (arg)
			(if (my-is-imagep arg)
				(my-print-image-eshell arg)
			  (eshell/cat arg)))
		  (-flatten args))
	nil))

(leaf esh-help
  :after eshell
  :elpaca t
  :require t
  :config
  (setup-esh-help-eldoc))

(leaf virtualenvwrapper
  :elpaca t)

(leaf eshell-prompt-extras
  :elpaca t
  :config
  (when (package-installed-p 'virtualenvwrapper)
	(progn
	  (require 'virtualenvwrapper)
	  (venv-initialize-eshell)))
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
  :elpaca t)

(leaf eshell-did-you-mean
  :after eshell
  :elpaca t
  :config
  (eshell-did-you-mean-setup))

(leaf eshell-syntax-highlighting
  :after eshell
  :elpaca t)

(leaf eat
  :after eshell
  :elpaca t
  :bind (:eat-mode-map
		 ("M-t" . execute-extended-command)
		 ("M-x" . execute-extended-command))
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

(provide 'k-eshell)
