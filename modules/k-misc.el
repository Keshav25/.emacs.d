;; User Information
(setq user-full-name "Keshav Italia"
      user-mail-address "keshavitalia0@gmail.com")

; Async
(leaf async
  :ensure t
  :init
  (dired-async-mode 1))

;; keep emacs clean
(leaf no-littering :ensure t)

;; Gcmh
(leaf gcmh
  :ensure t
  :config
  (gcmh-mode 1))

(provide 'k-misc)
