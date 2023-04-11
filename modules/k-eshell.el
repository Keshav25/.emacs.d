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
  nil)

(leaf esh-help
  :ensure t
  :require t
  :config
  (setup-esh-help-eldoc))

(leaf eshell-prompt-extras
  :ensure t
  :after eshell
  :config
  (with-eval-after-load "esh-opt"
	(when (package-installed-p 'virtualenvwrapper)
	  (progn
		(require 'virtualenvwrapper)
		(venv-initialize-eshell)))
	(autoload 'epe-theme-lambda "eshell-prompt-extras")
	(setq eshell-highlight-prompt nil
		  eshell-prompt-function 'epe-theme-multiline-with-status))

(leaf eshell-z
  :ensure t
  :after eshell
  :require t)

(leaf eshell-vterm
  :ensure t
  :after eshell
  :config
  (eshell-vterm-mode)
  (defalias 'eshell/v 'eshell-exec-visual))

(leaf eshell-bookmark
  :ensure t)

(leaf eshell-did-you-mean
  :ensure t
  :config
  (eshell-did-you-mean-setup))

(leaf eshell-syntax-highlighting
  :ensure t)



(provide 'k-eshell)
