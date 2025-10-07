(leaf org-aws-iam-role
  :doc "requires aws cli"
  :disabled t
  :elpaca (org-aws-iam-role :host github :repo "will-abb/org-aws-iam-role")
  :require t
  :custom
  (org-aws-iam-role-profile . "my-profile") ;; Use a specific AWS CLI profile
  (org-aws-iam-role-read-only-by-default . t) ;; Open buffers in read-only mode
  (org-aws-iam-role-show-folded-by-default . t) ;; Show Org buffer folded by default
  (org-aws-iam-role-fullscreen . nil) ;; Prevent the buffer from taking the full frame
  )
