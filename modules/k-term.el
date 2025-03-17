(leaf vterm)

(leaf exec-path-from-shell
  :when (not iswindows)
  :elpaca t
  :config
  (exec-path-from-shell-initialize))

(provide 'k-term)
