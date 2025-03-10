;; k-misc.el -- Miscellanious Configuration

;;; Commentary:

;; User Information
(setq user-full-name "Keshav Italia"
	  user-mail-address "keshavitalia0@gmail.com")

;; Async
(leaf async
  :elpaca t
  :init
  (dired-async-mode 1))

;; (use-package detached
;;   :elpaca t
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
;;   :elpaca t
;;   :ensure-system-package dtach
;;   :config
;;   (detached-init))

;; keep emacs clean
(leaf no-littering :elpaca t)

;; Gcmh
(leaf gcmh
  :elpaca t
  :config
  (gcmh-mode 1))

(leaf helpful
  :elpaca t
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
  :elpaca t)


(leaf elisp-demos
  :elpaca t
  :after (helpful)
  :require t
  :advice
  (:after helpful-update elisp-demos-advice-helpful-update))

(leaf elisp-autofmt
  :elpaca t
  :commands (elisp-autofmt-mode elisp-autofmt-buffer)
  :hook (emacs-lisp-mode . elisp-autofmt-mode))

(setq browse-url-browser-function 'browse-url-xdg-open)

(leaf ement
  :elpaca t)

(leaf plz
  :elpaca t)

(leaf plz-see
  :elpaca t)

(leaf verb
  :elpaca t)

(leaf mastodon
  :elpaca t
  :require t
  :bind
  ("C-c C-x C-n" . mastodon-toot)
  :setq
  (mastodon-instance-url . "https://mastodon.social/")
  (mastodon-active-user . "thepoetlogician")
  :config
  (mastodon-discover))

(leaf bbdb
  :elpaca t)

(leaf speed-type
  :elpaca t)

(leaf subed
  :elpaca t)

(leaf electric-ospl
  :elpaca t)

(leaf pair-tree
  :elpaca t)

(leaf jinx
  :elpaca t
  :bind (([remap ispell-word] . jinx-correct))
  :config
  (global-jinx-mode 1))

;; https://github.com/astoff/jit-spell
;; https://github.com/alphapapa/unpackaged.el

(leaf ts
  :elpaca t)

(leaf altcaps
  :elpaca t)

(leaf hyperdrive
  :elpaca t)

(leaf browser-hist
  :after embark
  :elpaca (browser-hist :host github :repo "agzam/browser-hist.el")
  :config
  (setq browser-hist-default-browser 'firefox)
  :commands (browser-hist-search))

(leaf atomic-chrome
  :doc "must have Chrome Emacs extension in Chromium installed"
  :elpaca (atomic-chrome
		   :repo "KarimAziev/atomic-chrome"
		   :host github)
  :commands (atomic-chrome-start-server)
  :config
  (setq-default atomic-chrome-extension-type-list '(atomic-chrome))
  (atomic-chrome-start-server))

(leaf casual
  :disabled t
  :doc "calculator program based on calc"
  :elpaca t)

(leaf number
  :elpaca t
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
  :elpaca t)

(leaf dir-config
  :elpaca (dir-config :host github :repo "jamescherti/dir-config.el")
  :custom
  (dir-config-file-names . '(".dir-config.el"))
  ;;(dir-config-allowed-directories '("~/src" "~/projects"))
  :config
  (dir-config-mode))

(leaf sotclojure
  :elpaca t)

(leaf gptel
  :elpaca t
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
  :elpaca t
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
  :elpaca (gptel-quick :host github :repo "karthink/gptel-quick")
  :bind (:embark-general-map
		 ("?" . #'gptel-quick)))


(leaf chatgpt-shell
  :elpaca t
  :custom
  (chatgpt-shell-model-version . "uncensored-dolphin-mistral:latest"))

(leaf casual-suite
  :elpaca t
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
  :elpaca t)

(leaf move-text
  :elpaca t
  :bind (([M-up] . move-text-up)
		 ([M-down] . move-text-down)))

(leaf emms
  :elpaca t
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

(leaf gif-screencast 
  :elpaca (gif-screencast :host gitlab
						  :repo "https://gitlab.com/ambrevar/emacs-gif-screencast") 
  :bind (("<f9>" . gif-screencast-start-or-stop)) 
  :custom (gif-screencast-program . "flameshot-bash") 
  (gif-screencast-args . '()))

(leaf detached
  :elpaca t
  :config
  (detached-init)
  :bind
  (([remap async-shell-command] . detached-shell-command)
   ([remap compile] . detached-compile)
   ([remap recompile] . detached-compile-recompile)
   ([remap detached-open-session] . detached-consult-session))
  :custom
  (detached-show-output-on-attach . t)
  (detached-terminal-data-command . system-type))


(provide 'k-misc)
