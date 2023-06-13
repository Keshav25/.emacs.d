(leaf magit
  :bind
  ("C-x g" . 'magit-status))

(leaf magit-delta
  :after magit
  :ensure t)

(leaf forge
  :ensure t)

(leaf diff-hl
  :ensure t)

(leaf git-gutter
  :ensure t)

(leaf code-review
  :ensure t)

(provide 'k-vc)
