(leaf lsp-go
  :after (lsp))

(leaf dap-go
  :after (dap)
  :config
  (require 'dap-dlv-go))

(provide 'k-go)
