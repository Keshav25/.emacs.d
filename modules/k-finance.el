
(leaf ledger-mode
  :ensure t)

(leaf evil-ledger
  :after (evil)
  :ensure t
  :hook ((ledger-mode-hook . evil-ledger-mode)))

(provide 'k-finance)
