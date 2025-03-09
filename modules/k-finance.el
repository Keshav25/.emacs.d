
(leaf ledger-mode
  :elpaca t)

(leaf evil-ledger
  :after (evil)
  :elpaca t
  :hook ((ledger-mode-hook . evil-ledger-mode)))

(provide 'k-finance)
