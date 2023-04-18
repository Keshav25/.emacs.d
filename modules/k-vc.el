(leaf magit
  :bind
  ("C-x g" . 'magit))

(leaf magit-delta
  :after magit
  :ensure t)

(leaf forge
  :ensure t)

(leaf diff-hl
  :ensure t)

(leaf git-gutter
  :ensure t)

(provide 'k-vc)
