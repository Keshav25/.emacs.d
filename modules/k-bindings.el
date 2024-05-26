(leaf embrace
  :ensure t)

(leaf multifiles
  :ensure t
  :require t
  :bind
  (("C-!" . mf/mirror-region-in-multifile)))
