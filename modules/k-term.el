(leaf vterm)

(leaf exec-path-from-shell
  :when (not iswindows)
  :elpaca t
  :custom
  (exec-path-from-shell-variables . '("PATH" "MANPATH" "LSP_USE_PLISTS"))
  :config
  (exec-path-from-shell-initialize))

(provide 'k-term)
