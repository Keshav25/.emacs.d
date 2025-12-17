;; -*- lexical-binding: t -*-

(leaf python-mode
  :custom ((python-indent-guess-indent-offset . t)
		   (python-indent-guess-indent-offset-verbose . nil)))

(leaf anaconda-mode
  :elpaca t
  :hook ((python-mode . anaconda-mode)))

(leaf blacken
  :elpaca t
  :hook ((python-mode . blacken-mode)))

(leaf eglot
  :custom
  (eglot-autoshutdown . t)
  (eglot-confirm-server-edits . nil)
  (eglot-report-progress . t)
  (eglot-extend-to-xref . t)
  (eglot-autoreconnect . t)
  :bind (("C-c d i" . eglot-find-implementation)
		 ("C-c d e" . eglot)
		 ("C-c d k" . eglot-shutdown-all)
		 ("C-c d r" . eglot-rename)
		 ("C-c d x" . eglot-reconnect)
		 ("C-c d a" . eglot-code-actions)
		 ("C-c d m" . eglot-menu)
		 ("C-c d f" . eglot-format-buffer)
		 ("C-c d h" . eglot-inlay-hints-mode)
		 (:eglot-diagnostic
		  ("M-RET" . eglot-code-actions)))
  :hook ((python-mode . eglot-ensure)))

(leaf pyvenv
  :elpaca t
  :hook ((python-mode . pyvenv-mode)
		 (python-mode . pyvenv-tracking-mode)
		 (pyvenv-post-activate-hooks . pyvenc-restart-python))
  :custom
  (pyvenv-default-virtual-env-name . "venv"))

(leaf numpydoc
  :elpaca t
  :hook ((python-mode . eldoc-mode)))

(leaf ein
  :elpaca t
  :setq
  (ein:polymode . t)
  (ein:output-area-inlined-images . t))

(provide 'k-python)
