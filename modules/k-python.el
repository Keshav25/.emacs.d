(leaf python-mode
  :custom ((python-indent-guess-indent-offset . t)
		   (python-indent-guess-indent-offset-verbose . nil)))

(leaf anaconda-mode
  :ensure t
  :hook ((python-mode . anaconda-mode)))

(leaf blacken
  :ensure t
  :hook ((python-mode . blacken-mode)))

(leaf eglot-mode
  :hook ((python-mode . eglot-ensure)))

(leaf pyvenv
  :ensure t
  :hook ((python-mode . pyvenv-mode)
		 (python-mode . pyvenv-tracking-mode)
		 (pyvenv-post-activate-hooks . pyvenc-restart-python))
  :custom
  (pyvenv-default-virtual-env-name . "venv"))

(leaf numpydoc
  :ensure t
  :hook ((python-mode . eldoc-mode)))

(provide 'k-python)
