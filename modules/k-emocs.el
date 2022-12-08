(leaf vertico
  :config
  (vertico-mode 1))

(leaf vertico-posframe
  :config
  ;; (vertico-posframe-mode 1)
)

(leaf orderless
      :config
      (setq completion-styles '(orderless)))

(leaf marginalia
  :config
  (marginalia-mode 1))

(leaf consult
  :bind ("C-s" . consult-line))


(leaf corfu)
(leaf cape)

(provide 'k-emocs)
