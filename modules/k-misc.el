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
  :bind
  ("C-c C-x C-n" . mastodon-toot)
  :setq
  (mastodon-instance-url . "https://mastodon.social/")
  (mastodon-active-user . "thepoetlogician")
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
  :ensure t
  :bind (([remap ispell-word] . jinx-correct))
  :config
  (global-jinx-mode 1))

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
  :disabled t
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

(leaf gptel
  :ensure t
  :config
  (setq gptel-default-mode #'org-mode)
  (setq
   gptel-model 'uncensored-dolphin-mistral:latest
   gptel-backend (gptel-make-ollama "Ollama"
                   :host "localhost:11434"
                   :stream t
                   :models '(uncensored-dolphin-mistral:latest))))

(leaf corsair
  :after gptel
  :ensure t
  :bind
  ("C-c g c" . corsair-open-chat-buffer)
  ("C-c g a c" . corsair-accumulate-file-path-and-contents)
  ("C-c g a n" . corsair-accumulate-file-name)
  ("C-c g a v" . corsair-accumulate-file-path)
  ("C-c g a w" . corsair-accumulate-selected-text)
  ("C-c g a D" . corsair-drop-accumulated-buffer)
  ("C-c g f" . corsair-insert-file-or-folder-contents))

;; doesn't install
(leaf gptel-quick
  :disabled t
  :quelpa (gptel-quick :fetcher github :repo "karthink/gptel-quick")
  :bind (:embark-general-map
		 ("?" . #'gptel-quick)))


(leaf chatgpt-shell
  :ensure t
  :custom
  (chatgpt-shell-model-version . "uncensored-dolphin-mistral"))

(leaf casual-suite
  :ensure t
  :config
  (require  'casual-image)
  ;; Add Meme Commands
  (defun k-make-meme ()
	(async-shell-command "convert temp.jpg -gravity North -pointsize 30 -annotate +0+100 'Love you mom' temp1.jpg "))
  (transient-insert-suffix 'casual-image-tmenu "c"
	'("M" "add a caption" image-increase-size))
  :bind (:image-mode
		 ("C-o" . casual-image-tmenu)))

(leaf emojs
  :bind
  ("C-c i e" . emoji-insert))

(leaf hiccup-cli
  :ensure t)

(leaf move-text
  :ensure t
  :bind (([M-up] . move-text-up)
		 ([M-down] . move-text-down)))

(leaf emms
  :ensure t
  :config
  (require 'emms-setup)
  (require 'emms-mpris)
  (emms-all)
  (emms-default-players)
  (emms-mpris-enable)
  :bind
  ("<XF86AudioPrev>" . emms-previous)
  ("<XF86AudioNext>" . emms-next)
  ("<XF86AudioPlay>" . emms-pause))

(provide 'k-misc)
