;; k-misc.el -- Miscellanious Configuration

;;; Commentary:

;; User Information
(setq user-full-name "Keshav Italia"
	  user-mail-address "keshavitalia0@gmail.com")

;; Async
(leaf async
  :ensure t
  :init
  (dired-async-mode 1))

;; (use-package detached
;;   :ensure t
;;   ;; :ensure-system-package "dtach"
;;   :init
;;   (detached-init)
;;   :bind (;; Replace `async-shell-command' with `detached-shell-command'
;;          ([remap async-shell-command] . detached-shell-command)
;;          ;; Replace `compile' with `detached-compile'
;;          ([remap compile] . detached-compile)
;;          ([remap recompile] . detached-compile-recompile)
;;          ;; Replace built in completion of sessions with `consult'
;;          [remap detached-open-session] . detached-consult-session))
;;   :custom ((detached-show-output-on-attach t)
;;            (detached-terminal-data-command system-type)))

;; (leaf detached
;;   :ensure t
;;   :ensure-system-package dtach
;;   :config
;;   (detached-init))

;; keep emacs clean
(leaf no-littering :ensure t)

;; Gcmh
(leaf gcmh
  :ensure t
  :config
  (gcmh-mode 1))

(leaf helpful
  :ensure t
  :bind
  ("C-h f" . 'helpful-callable)
  ("C-h v" . 'helpful-variable)
  ("C-h k" . 'helpful-key)
  ("C-h x" . 'helpful-command)
  ("C-h o" . 'helpful-symbol)
  ("C-h RET" . 'helpful-at-point)
  ("C-h F" . 'helpful-function)
  ;; not helpful but might aswell
  ("C-h K" . 'describe-keymap)
  (:helpful-mode-map
   ([remap revert-buffer] . 'helpful-update)))

(leaf inform
  :ensure t)


(leaf elisp-demos
  :ensure t
  :after (helpful)
  :require t
  :advice
  (:after helpful-update elisp-demos-advice-helpful-update))

(leaf elisp-autofmt
  :ensure t
  :commands (elisp-autofmt-mode elisp-autofmt-buffer)
  :hook (emacs-lisp-mode . elisp-autofmt-mode))

(setq browse-url-browser-function 'browse-url-xdg-open)

(leaf ement
  :ensure t)

(leaf plz
  :ensure t)

(leaf plz-see
  :ensure t)

(leaf verb
  :ensure t)

(leaf mastodon
  :ensure t
  :require t
  :config
  (mastodon-discover))

(leaf bbdb
  :ensure t)

(leaf speed-type
  :ensure t)

(leaf subed
  :ensure t)

(leaf electric-ospl
  :ensure t)

(leaf pair-tree
  :ensure t)

(leaf jinx
  :ensure t)

;; https://github.com/astoff/jit-spell
;; https://github.com/alphapapa/unpackaged.el

(leaf ts
  :ensure t)

(leaf altcaps
  :ensure t)

(leaf hyperdrive
  :ensure t)

(leaf browser-hist
  :after embark
  :quelpa (browser-hist :fetcher github :repo "agzam/browser-hist.el")
  :config
  (setq browser-hist-default-browser 'firefox)
  :commands (browser-hist-search))

(leaf atomic-chrome
  :doc "must have Chrome Emacs extension in Chromium installed"
  :quelpa (atomic-chrome
		   :repo "KarimAziev/atomic-chrome"
		   :fetcher github)
  :commands (atomic-chrome-start-server)
  :config
  (setq-default atomic-chrome-extension-type-list '(atomic-chrome))
  (atomic-chrome-start-server))

(leaf casual
  :doc "calculator program based on calc"
  :ensure t)

(leaf number
  :ensure t
  :require t
  ;; example keybindings
  ;; (global-set-key (kbd "C-c C-+") 'number/add)
  ;; (global-set-key (kbd "C-c C--") 'number/sub)
  ;; (global-set-key (kbd "C-c C-*") 'number/multiply)
  ;; (global-set-key (kbd "C-c C-/") 'number/divide)
  ;; (global-set-key (kbd "C-c C-0") 'number/pad)
  ;; (global-set-key (kbd "C-c C-=") 'number/eval)
  )

(leaf expreg
  :doc "alternative to expand-region that defers to treesitter when possible"
  :ensure t)

(leaf dir-config
  :quelpa (dir-config :fetcher github :repo "jamescherti/dir-config.el")
  :custom
  (dir-config-file-names . '(".dir-config.el"))
  ;;(dir-config-allowed-directories '("~/src" "~/projects"))
  :config
  (dir-config-mode))

(leaf sotclojure
  :ensure t)


(provide 'k-misc)

