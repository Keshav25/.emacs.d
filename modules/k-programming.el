(leaf eglot)

(leaf flymake
  :ensure t
  :config
  (leaf flymake-collections
	:ensure t))

(leaf envrc
  :ensure t)

(leaf treemacs
  :ensure t
  :setq
  (treemacs-width . 25)
  :hook (treemacs-mode-hook . '(text-scale-adjust -1)))


(provide 'k-programming)
