;; -*- lexical-binding: t -*-

;;; Code:

(leaf prog-mode
  :hook ((prog-mode-hook . subword-mode)
		 (prog-mode-hook . (lambda () (setq-local fill-column 120)))))

(leaf eglot
  :disabled t
  :hook ((rust-mode-hook . eglot-ensure))
  :bind (:eglot-mode-map
		 ("C-c e r" . #'eglot-rename)
		 ("C-<down-mouse-1>" . #'xref-find-definitions)
		 ("C-S-<down-mouse-1>" . #'xref-find-references)
		 ("C-c C-c" . #'eglot-code-actions))
  :custom
  (eglot-autoshutdown . t))

(leaf consult-eglot
  :after (eglot)
  :disabled t
  :elpaca t
  :bind (:eglot-mode-map ("s-t" . #'consult-eglot-symbols)))

(leaf treesit-auto
  :elpaca t
  :require t
  :setq
  (treesit-auto-install . 'prompt)
  :config
  (global-treesit-auto-mode))

(leaf combobulate
  :elpaca (combobulate :host github :repo "mickeynp/combobulate")
  :config
  (setq combobulate-key-prefix "C-c o")
  :hook
  ((python-ts-mode-hook . combobulate-mode)
   (js-ts-mode-hook . combobulate-mode)
   (go-ts-mode-hook . combobulate-mode)))

(leaf envrc
  :elpaca t
  :config
  (envrc-global-mode))

(leaf flycheck
  :elpaca t
  :config
  (global-flycheck-mode 1)
  
  (leaf avy-flycheck
	:after (flycheck avy)
	:elpaca t)
  
  (leaf flycheck-eglot
	:after (flycheck eglot)
	:elpaca t)
  
  (leaf flycheck-guile
	:after (flycheck)
	:elpaca t)
  
  (leaf flycheck-ledger
	:after (flycheck)
	:elpaca t)
  
  (leaf consult-flycheck
	:after (flycheck consult)
	:elpaca t)
  
  (leaf flycheck-inline
	:after (flycheck)
	:elpaca t))

(leaf flyover
  :ensure t
  :require t
  :hook (flycheck-mode . flyover-mode)
  :custom
  (flyover-levels . '(error warning info))
  (flyover-use-theme-colors . t))

(leaf treemacs
  :elpaca t
  :setq
  (treemacs-width . 25))

(leaf aggressive-indent
  :elpaca t
  :hook ((emacs-lisp-mode-hook . aggressive-indent-mode)
		 (scheme-mode . aggressive-indent-mode)))

(leaf rainbow-delimiters
  :elpaca t
  :hook
  (prog-mode-hook . (rainbow-delimiters-mode))
  (text-mode-hook . (rainbow-delimiters-mode)))

(leaf prism
  :disabled t
  :elpaca t
  :after ef-themes
  :config
  (prism-mode 1)
  (prism-set-colors
   :desaturations '(0) ; do not change---may lower the contrast ratio
   :lightens '(0)      ; same
   :colors (ef-themes-with-colors
            (list fg-main
				  magenta
				  cyan-cooler
				  magenta-cooler
				  blue
				  magenta-warmer
				  cyan-warmer
				  red-cooler
				  green
				  fg-main
				  cyan
				  yellow
				  blue-warmer
				  red-warmer
				  green-cooler
				  yellow-faint))))

(leaf paredit
  :elpaca t
  :hook (emacs-lisp-mode-hook . enable-paredit-mode))

(leaf paredit-everywhere
  ;; Original is by Steve Purcell
  :after (paredit)
  :config
  (require 'paredit)
  
  (defvar paredit-everywhere-mode-map
	(let ((m (make-sparse-keymap)))
	  (define-key m (kbd "C-)") 'paredit-forward-slurp-sexp)
	  (define-key m (kbd "C-}") 'paredit-forward-barf-sexp)
	  (define-key m (kbd "M-(") 'paredit-wrap-round)
	  (define-key m (kbd "M-)") 'paredit-close-round-and-newline)
	  (define-key m (kbd "M-]") 'paredit-close-square-and-newline)
	  (define-key m (kbd "M-\"") 'paredit-meta-doublequote)
	  (define-key m (kbd "M-S") 'paredit-split-sexp)
	  (define-key m (kbd "M-J") 'paredit-join-sexps)
	  (define-key m (kbd "M-s") 'paredit-splice-sexp)
	  (define-key m (kbd "M-r") 'paredit-raise-sexp)
	  (define-key m (kbd "M-DEL") 'paredit-backward-kill-word)
	  (define-key m (kbd "M-d") 'paredit-forward-kill-word)
	  (define-key m (kbd "C-k") 'paredit-kill)
	  (global-set-key "\M-[" #'paredit-wrap-square)
	  (global-set-key "\M-{" #'paredit-wrap-curly)
	  (global-set-key (kbd "C-M-u") #'paredit-backward-up)
	  (global-set-key (kbd "C-M-n") #'paredit-forward-up)
	  ;; This one's surpisingly useful for writing prose.
	  (global-set-key "\M-S"
					  #'paredit-splice-sexp-killing-backward)
	  m)
	"Keymap for `paredit-everywhere-mode'.")
  
;;;###autoload
  (define-minor-mode paredit-everywhere-mode
	"A cut-down version of paredit which can be used in non-lisp buffers."
	:lighter " Par-"
	:keymap paredit-everywhere-mode-map)
  
  (defun turn-off-paredit-everywhere-mode ()
	"Disable `paredit-everywhere-mode'."
	(paredit-everywhere-mode 0))
  
  ;; Disable paredit-everywhere when full paredit is enabled
  (add-hook 'paredit-mode-hook 'turn-off-paredit-everywhere-mode)
  (paredit-everywhere-mode 1)
  )

(leaf paredit-menu
  :elpaca t)

(leaf moldable-emacs
  :disabled t
  :init
  (require 'f)
  (if (f-directory-p "~/.emacs.d/site-lisp/moldable-emacs")
      (shell-command "cd ~/.emacs.d/site-lisp/moldable-emacs; git pull;")
    (shell-command "cd ~/.emacs.d/site-lisp/; git clone git@github.com:ag91/moldable-emacs.git"))
  :load-path "~/.emacs.d/site-lisp/moldable-emacs/"
  :bind (("C-c v m" . me-mold)
         ("C-c v f" . me-go-forward)
         ("C-c v b" . me-go-back)
         ("C-c v o" . me-open-at-point)
         ("C-c v d" . me-mold-docs)
         ("C-c v g" . me-goto-mold-source)
         ("C-c v e a" . me-mold-add-last-example))
  :require t
  :config
  (add-to-list 'me-files-with-molds (concat (file-name-directory (symbol-file 'me-mold)) "molds/experiments.el")) ;; TODO this is relevant only if you have private molds
  (me-setup-molds))

(leaf lsp-mode
  :elpaca t
  :require t
  :init
  (setq lsp-keymap-prefix "C-c C-y")
  :custom
  (lsp-eldoc-render-all . t)
  (lsp-idle-delay . 0.6)
  (lsp-inlay-hint-enable . t)
  :hook
  (lsp-mode-hook . lsp-ui-mode)
  (lsp-mode . lsp-enable-which-key-integration)
  (python-ts-mode . lsp))

(leaf lsp-ui
  :elpaca t
  :custom
  (lsp-ui-peek-always-show . t)
  (lsp-ui-sideline-show-hover . t)
  (lsp-ui-doc-use-webkit . t)
  (lsp-ui-doc-enable . nil))

(leaf lsp-installer
  :elpaca (lsp-installer :host github
						 :repo "kn66/lsp-installer.el")
  :config
  (lsp-installer-setup))

(leaf dap-mode
  :elpaca t
  :bind (:dap-mode-map
		 ("C-c d b" . dap-breakpoint-toggle)
		 ("C-c d r" . dap-debug-restart)
		 ("C-c d l" . dap-debug-last)
		 ("C-c d d" . dap-debug))
  :config
  (dap-ui-mode)
  (dap-ui-controls-mode 1)
  
  (require 'dap-lldb)
  (require 'dap-gdb-lldb)
  ;; installs .extension/vscode
  (dap-gdb-lldb-setup))

(leaf direnv
  :elpaca t
  :config (direnv-mode))

(leaf tramp
  :require t
  :setq
  (tramp-default-method . "ssh")
  (tramp-verbose . 1)
  (tramp-default-remote-shell . "/bin/bash")
  (tramp-connection-local-default-shell-variables .
												  '((shell-file-name . "/bin/bash")
													(shell-command-switch . "-c")))
  :config
  (connection-local-set-profile-variables 'tramp-connection-local-default-shell-profile
										  '((shell-file-name . "/bin/bash")
											(shell-command-switch . "-c")))
  
  ;; add gh codespaces ssh method support for tramp editing
  ;; e.g. C-x C-f /ghcs:codespace-name:/path/to/file
  ;; thanks to my coworker Bas for this one
  (let ((ghcs (assoc "ghcs" tramp-methods))
		(ghcs-methods '((tramp-login-program "gh")
						(tramp-login-args (("codespace") ("ssh") ("-c") ("%h")))
						(tramp-remote-shell "/bin/sh")
						(tramp-remote-shell-login ("-l"))
						(tramp-remote-shell-args ("-c")))))
	;; just for debugging the methods
	(if ghcs (setcdr ghcs ghcs-methods)
	  (push (cons "ghcs" ghcs-methods) tramp-methods))))

(leaf tramp-lsp
  :after (tramp lsp)
  :disabled t
  :config
  ;; (lsp-register-client
  ;;  (make-lsp-client :new-connection (lsp-stdio-connection "gopls")
  ;; 					:major-modes '(go-mode go-dot-mod-mode)
  ;; 					:language-id "go"
  ;; 					:remote? t
  ;; 					:priority 0
  ;; 					:server-id 'gopls-remote
  ;; 					:completion-in-comments? t
  ;; 					:library-folders-fn #'lsp-go--library-default-directories
  ;; 					:after-open-fn (lambda ()
  ;; 									 ;; https://github.com/golang/tools/commit/b2d8b0336
  ;; 									 (setq-local lsp-completion-filter-on-incomplete nil))))
  )

(leaf restclient
  :elpaca t
  :mode  ("\\.restclient$" . restclient-mode))

(leaf yaml-mode
  :elpaca t)

(leaf dockerfile-mode
  :elpaca t)

(leaf toml-mode
  :elpaca t)

(leaf dape
  :elpaca t
  :url "https://github.com/svaante/dape")

(leaf apheleia
  :elpaca t
  :doc "reformats code on file save"
  :config
  (apheleia-global-mode 1))

(leaf turbo-log
  :elpaca (turbo-log :host github :repo "artawower/turbo-log.el"))

(leaf macrursors
  :disabled t  
  :elpaca (macrusors :host github :repo "corytertel/macrursors")
  :require t
  :config
  (dolist (mode '(corfu-mode goggles-mode beacon-mode))
	(add-hook 'macrursors-pre-finish-hook mode)
	(add-hook 'macrursors-post-finish-hook mode))
  (define-prefix-command 'macrursors-mark-map)
  (global-set-key (kbd "C-c SPC") #'macrursors-select)
  (global-set-key (kbd "C->") #'macrursors-mark-next-instance-of)
  (global-set-key (kbd "C-<") #'macrursors-mark-previous-instance-of)
  (global-set-key (kbd "C-;") 'macrursors-mark-map)
  (define-key macrursors-mark-map (kbd "C-;") #'macrursors-mark-all-lines-or-instances)
  (define-key macrursors-mark-map (kbd ";") #'macrursors-mark-all-lines-or-instances)
  (define-key macrursors-mark-map (kbd "l") #'macrursors-mark-all-lists)
  (define-key macrursors-mark-map (kbd "s") #'macrursors-mark-all-symbols)
  (define-key macrursors-mark-map (kbd "e") #'macrursors-mark-all-sexps)
  (define-key macrursors-mark-map (kbd "f") #'macrursors-mark-all-defuns)
  (define-key macrursors-mark-map (kbd "n") #'macrursors-mark-all-numbers)
  (define-key macrursors-mark-map (kbd ".") #'macrursors-mark-all-sentences)
  (define-key macrursors-mark-map (kbd "r") #'macrursors-mark-all-lines))

(leaf devdocs
  :elpaca t
  :bind ("C-h D" . 'devdocs-lookup))

(leaf code-cells
  :elpaca t)

(leaf buffer-env
  :elpaca t)

(leaf obvious
  :elpaca (obvious :host github :repo "alphapapa/obvious.el"))

(leaf repl-driven-development
  :elpaca t
  :require t
  :config
  (repl-driven-development [C-x C-j] java)       
  (repl-driven-development [C-x C-n] javascript) 
  (repl-driven-development [C-x C-p] python)
  (repl-driven-development [C-x C-t] terminal))

(leaf cider
  :elpaca t
  :require t
  :custom
  (nrepl-use-ssh-fallback-for-remote-hosts . t)
  :config
  (autoload 'cider--make-result-overlay "cider-overlays")
  
  (defun k/eval-overlay (value point)
	"Evaluate Elisp with a Cider Overlay"
	(cider--make-result-overlay (format "%S" value)
	  :where point
	  :duration 'command)
	value)
  
  (advice-add 'eval-region :around
			  (lambda (func beg end &rest region)
				(k/eval-overlay
				 (apply func beg end region)
				 end)))
  
  (advice-add 'eval-last-sexp :filter-return
			  (lambda (region)
				(k/eval-overlay region (point))))
  
  (advice-add 'eval-defun :filter-return
			  (lambda (region)
				(k/eval/overlay region
								(save-excursion
								  (end-of-defun)
								  (point))))))

(leaf rainbow-mode
  :elpaca t)

(leaf yasnippet
  :elpaca t
  :require t
  :bind
  ("C-<escape>" . yas-expand)
  :config
  (yas-global-mode)
  (yas-reload-all)
  :hook
  (prog-mode-hook . yas-minor-mode)
  (text-mode-hook . yas-minor-mode)
  :custom
  (yas-prompt-functions . '(yas-completing-prompt)))

(leaf yasnippet-snippets
  :elpaca t)

(leaf consult-yasnippet
  :elpaca t)

(leaf elisp-slime-nav
  :elpaca t
  :hook ((emacs-lisp-mode-hook . elisp-slime-nav-mode)))

(leaf compile
  :bind (("C-c y" . 'compile)
		 ("<f5>" . 'recompile))
  :custom
  (compilation-scroll-output . t)
  (compilation-always-kill . t)
  (compilation-scroll-output . 'first-error)
  (next-error-recenter . '(4))
  :config
  (defadvice compile (before ad-compile-smart activate)
	"Advises `compile' so it sets the argument COMINT to t."
	(ad-set-arg 1 t))
  :hook
  (compilation-filter-hook . ansi-color-compilation-filter)
  (eshell-load-hook . compilation-shell-minor-mode)
  (shell-mode-hook . compilation-shell-minor-mode)
  (term-load-hook . compilation-shell-minor-mode))

(leaf fancy-compilation
  :elpaca t
  :custom
  (fancy-compilation-override-colors . t)
  :config
  (fancy-compilation-mode 1))

(leaf symbol-overlay
  :elpaca t
  :hook ((text-mode-hook . symbol-overlay-mode)
		 (prog-mode-hook . symbol-overlay-mode)))

(leaf symbol-overlay-mc
  :elpaca t)

(leaf smartparens
  :elpaca t
  :require smartparens-config
  :hook ((prog-mode . smartparens-mode)))

(leaf outline-indent
  :elpaca t
  :commands outline-indent-minor-mode
  :custom
  (outline-indent-ellipsis . " ▼ "))

(leaf disproject
  :elpaca t
  ;; Replace `project-prefix-map' with `disproject-dispatch'.
  :custom
  (disproject-switch-to-buffer-command . 'consult-project-buffer)
  :bind (:ctl-x-map
         ("p" . disproject-dispatch)))

(leaf org-project-capture
  :elpaca t)

(leaf lispy
  :elpaca t
  :doc "https://github.com/abo-abo/lispy?tab=readme-ov-file#ide-like-features"
  ;; TODO: Relace Vim-Like bindings with Emacs ones
  :hook ((emacs-lisp-mode-hook . lispy-mode))
  :custom
  (lispy-compat . '(edebug cider))
  :bind (:lispy-mode-map
		 ("C-y" . 'consult-yank-from-kill-ring)
		 ("n" . 'special-lispy-down)	
		 ("p" . 'special-lispy-up)	
		 ("f" . 'special-lispy-right)
		 ("b" . 'special-lispy-left)
		 ;; originally lispy-left-maybe
		 ("M-o" . 'ace-window)))


(leaf eldoc
  :setq
  (eldoc-documentation-strategy . 'eldoc-documentation-default))

(leaf hideshow
  :hook ((prog-mode . hs-minor-mode))
  :config
  (defun toggle-fold ()
	(interactive)
	(save-excursion
      (end-of-line)
      (hs-toggle-hiding))))

(leaf eglot-inactive-regions
  :elpaca t
  :require t
  :custom
  (eglot-inactive-regions-style . 'darken-foreground)
  (eglot-inactive-regions-opacity . 0.4)
  :config
  (eglot-inactive-regions-mode 1))

(leaf bookmark-in-project
  :elpaca t)

(leaf eval-in-repl
  ;; This file alone is not functional. Also require the following depending
  ;; on your needs.

  ;; eval-in-repl-ielm.el    for Emacs Lisp    (via ielm)
  ;; eval-in-repl-cider.el   for Clojure       (via cider.el)
  ;; eval-in-repl-slime.el   for Common Lisp   (via slime.el)
  ;; eval-in-repl-geiser.el  for Racket/Scheme (via geiser.el)
  ;; eval-in-repl-racket.el  for Racket        (via racket-mode.el)
  ;; eval-in-repl-scheme.el  for Scheme        (via scheme.el and cmuscheme.el)
  ;; eval-in-repl-hy.el      for Hy            (via hy-mode.el and inf-lisp.el)

  ;; eval-in-repl-python.el  for Python        (via python.el)
  ;; eval-in-repl-ruby.el    for Ruby          (via ruby-mode.el, and inf-ruby.el)
  ;; eval-in-repl-sml.el     for Standard ML   (via sml-mode.el)
  ;; eval-in-repl-ocaml.el   for OCaml         (via tuareg.el)
  ;; eval-in-repl-prolog.el  for Prolog        (via prolog.el)
  ;; eval-in-repl-javascript.el for Javascript (via js3-mode.el, js2-mode.el, and js-comint.el)

  ;; eval-in-repl-shell.el   for Shell         (via native shell support)

  :elpaca t)

(leaf org-babel-eval-in-repl
  :elpaca t)

(leaf sideline-eglot
  :elpaca t)

(leaf sideline
  :after sideline-eglot
  :elpaca t
  :config
  (setq sideline-backends-right '(sideline-eglot)))


(provide 'k-programming)
