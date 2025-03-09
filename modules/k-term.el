(leaf vterm)

(leaf exec-path-from-shell
  :when (not iswindows)
yy  :elpaca t
  :config
  (exec-path-from-shell-initialize))

(provide 'k-term)
