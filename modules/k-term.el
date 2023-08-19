(leaf vterm)

(leaf exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(provide 'k-term)
