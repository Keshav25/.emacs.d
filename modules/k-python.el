(leaf python-mode
  :custom ((python-indent-guess-indent-offset . t)
		   (python-indent-guess-indent-offset-verbose . nil))

  :init
  (leaf lsp-python-ms
	:after lsp-mode
	:custom ((lsp-python-ms-auto-install-server . t)))
  :hook (python-mode-hook . (lambda ()
							  (require 'lsp-python-ms)
							  (lsp-deferred))))

(provide 'k-python)
