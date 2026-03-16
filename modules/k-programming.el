;; -*- lexical-binding: t -*-

;;; Code:

(leaf prog-mode
  :hook ((prog-mode-hook . subword-mode)
		 (prog-mode-hook . (lambda () (setq-local fill-column 120)))))

(leaf treesit-auto
  :elpaca t
  :require t
  :setq
  (treesit-auto-install . 'prompt)
  :config
  (global-treesit-auto-mode))

(leaf combobulate
  :elpaca (combobulate :host github :repo "mickeynp/combobulate")
  :custom
  (combobulate-key-prefix . "C-c o")
  :hook
  ((python-ts-mode-hook . combobulate-mode)
   (js-ts-mode-hook . combobulate-mode)
   (tsx-ts-mode-hook . combobulate-mode)
   (typescript-ts-mode-hook . combobulate-mode)
   (css-ts-mode-hook . combobulate-mode)
   (yaml-ts-mode-hook . combobulate-mode)
   (json-ts-mode-hook . combobulate-mode)
   (rust-ts-mode-hook . combobulate-mode)
   (go-ts-mode-hook . combobulate-mode)))

(leaf envrc
  :elpaca t
  :config
  (envrc-global-mode))

(leaf flycheck
  :elpaca t
  :require t
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
  :elpaca t
  :require t
  :custom
  (flyover-levels . '(error warning info))
  (flyover-use-theme-colors . t))

(leaf treemacs
  :elpaca t
  :custom
  (treemacs-width . 25)
  (treemacs-is-never-other-window . t)
  (treemacs-show-hidden-files . t)
  (treemacs-follow-after-init . t)
  :config
  (treemacs-follow-mode 1)
  (treemacs-filewatch-mode 1)
  (treemacs-git-mode 'deferred)
  (treemacs-fringe-indicator-mode 'always))

(leaf treemacs-nerd-icons
  :after treemacs
  :elpaca t
  :config
  (treemacs-load-theme "nerd-icons"))

(leaf aggressive-indent
  :elpaca t
  :hook ((emacs-lisp-mode-hook . aggressive-indent-mode)
		 (scheme-mode-hook . aggressive-indent-mode)))

(leaf rainbow-delimiters
  :elpaca t
  :hook
  (prog-mode-hook . (rainbow-delimiters-mode))
  (text-mode-hook . (rainbow-delimiters-mode)))

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

(leaf lsp-mode
  :elpaca t
  :custom
  (lsp-keymap-prefix . "C-c C-y")
  (lsp-eldoc-render-all . t)
  (lsp-idle-delay . 0.6)
  (lsp-inlay-hint-enable . t)
  (lsp-headerline-breadcrumb-enable . t)
  (lsp-modeline-diagnostics-enable . t)
  (lsp-modeline-code-actions-enable . t)
  (lsp-completion-provider . :none) ;; use corfu instead
  (lsp-enable-snippet . t)
  (lsp-enable-on-type-formatting . nil)
  (lsp-semantic-tokens-enable . t)
  (lsp-lens-enable . t)
  :hook
  (lsp-mode-hook . lsp-ui-mode)
  (lsp-mode-hook . lsp-enable-which-key-integration)
  (lsp-completion-mode-hook . (lambda ()
								 (setq-local completion-at-point-functions
										   (list (cape-capf-super #'lsp-completion-at-point #'cape-dabbrev)))))
  (python-ts-mode-hook . lsp))

(leaf lsp-ui
  :elpaca t
  :custom
  (lsp-ui-peek-always-show . t)
  (lsp-ui-sideline-show-hover . t)
  (lsp-ui-sideline-show-diagnostics . t)
  (lsp-ui-sideline-show-code-actions . t)
  (lsp-ui-doc-enable . t)
  (lsp-ui-doc-position . 'at-point)
  (lsp-ui-doc-delay . 0.5)
  (lsp-ui-doc-show-with-cursor . t)
  (lsp-ui-doc-show-with-mouse . nil)
  :bind (:lsp-ui-mode-map
		 ("C-c l p" . lsp-ui-peek-find-definitions)
		 ("C-c l r" . lsp-ui-peek-find-references)
		 ("C-c l i" . lsp-ui-peek-find-implementation)))

(leaf lsp-installer
  :after (eglot)
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
  :bind (("C-x C-j" . java-eval)
		 ("C-x C-n" . javascript-eval)
		 ("C-x C-p" . python-eval)
		 ("C-x C-t" . terminal-eval)))

;; TODO: Switch to Eros
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
				(k/eval-overlay region
								(save-excursion
								  (end-of-defun)
								  (point))))))

(leaf rainbow-mode
  :elpaca t)

(leaf yasnippet
  :elpaca t
  :bind
  ("C-<escape>" . yas-expand)
  :config
  (yas-reload-all)
  :hook
  (prog-mode-hook . yas-minor-mode)
  (text-mode-hook . yas-minor-mode)
  :custom
  (yas-prompt-functions . '(yas-completing-prompt))
  (yas-verbosity . 1))

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
  (compilation-always-kill . t)
  (compilation-scroll-output . 'first-error)
  (next-error-recenter . '(4))
  :config
  (advice-add 'compile :around
			 (lambda (orig-fn command &optional comint)
			   "Always use COMINT mode for compilation."
			   (funcall orig-fn command t)))
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

(leaf multiple-cursors
  :elpaca t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
		 ("C->" . mc/mark-next-like-this)
		 ("C-<" . mc/mark-previous-like-this)
		 ("C-c C-<" . mc/mark-all-like-this)))

(leaf smartparens
  :elpaca t
  :require smartparens-config
  :hook ((prog-mode-hook . smartparens-mode)
		 (conf-mode-hook . smartparens-mode))
  :config
  (sp-use-smartparens-bindings)
  (show-smartparens-global-mode 1))

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
  :after hideshow
  :elpaca t
  :doc "https://github.com/abo-abo/lispy?tab=readme-ov-file#ide-like-features"
  ;; TODO: Relace Vim-Like bindings with Emacs ones
										;:((emacs-lisp-mode-hook . lispy-mode))
  ;; :custom
  ;; (lispy-compat . '(edebug cider))
  :custom
  (lispy-compat . '(edebug cider))
  :bind (:lispy-mode-map
		 ("C-y" . 'consult-yank-from-kill-ring)
		 ("n" . 'special-lispy-down)
		 ("p" . 'special-lispy-up)
		 ("f" . 'special-lispy-right)
		 ("b" . 'special-lispy-left)
		 ("l" . 'special-lispy-view)
		 ;; originally lispy-left-maybe
		 ("M-o" . 'ace-window))
  ;; (:lispy-mode-map-special
  ;; ("<tab>" . k-toggle-fold))
  )

(leaf eldoc
  :custom
  (eldoc-documentation-strategy . 'eldoc-documentation-compose-eagerly)
  (eldoc-idle-delay . 0.3)
  (eldoc-echo-area-use-multiline-p . 3))

(leaf hideshow
  :hook ((prog-mode-hook . hs-minor-mode))
  :config
  (defun k-toggle-fold ()
	(interactive)
	(save-excursion
	  (end-of-line)
	  (hs-toggle-hiding))))

(leaf eglot-inactive-regions
  :elpaca t
  :custom
  (eglot-inactive-regions-style . 'darken-foreground)
  (eglot-inactive-regions-opacity . 0.4)
  :hook (c-mode . eglot-inactive-regions-mode))
;; TODO: Integrate with Disproject
(leaf bookmark-in-project
  :elpaca t
  :bind (("C-c p R" . bookmark-in-project-jump)))

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

(leaf minitest
  :elpaca t)

(leaf indent-bars
  :elpaca t
  :hook (prog-mode-hook . indent-bars-mode))


;; ============================================================
;; Enhanced programming environment
;; ============================================================

;; Highlight TODO/FIXME/HACK/NOTE in comments
(leaf hl-todo
  :elpaca t
  :custom
  (hl-todo-keyword-faces . '(("TODO"  . "#FF8C00")
							 ("FIXME" . "#FF0000")
							 ("HACK"  . "#FF4500")
							 ("NOTE"  . "#1E90FF")
							 ("BUG"   . "#FF0000")
							 ("XXX"   . "#FF4500")
							 ("PERF"  . "#DA70D6")))
  :hook (prog-mode-hook . hl-todo-mode)
  :bind (:hl-todo-mode-map
		 ("C-c t p" . hl-todo-previous)
		 ("C-c t n" . hl-todo-next)
		 ("C-c t o" . hl-todo-occur)
		 ("C-c t i" . hl-todo-insert)))

;; Show git diff indicators in the fringe
(leaf diff-hl
  :elpaca t
  :hook ((prog-mode-hook . diff-hl-mode)
		 (dired-mode-hook . diff-hl-dired-mode)
		 (magit-pre-refresh-hook . diff-hl-magit-pre-refresh)
		 (magit-post-refresh-hook . diff-hl-magit-post-refresh))
  :config
  (global-diff-hl-mode 1)
  (diff-hl-flydiff-mode 1))

;; Respect .editorconfig project settings
(leaf editorconfig
  :elpaca t
  :config
  (editorconfig-mode 1))

;; Show current function name in modeline
(leaf which-func
  :config
  (which-function-mode 1))

;; Colorize color strings in buffers (#ff0000, rgb(...), etc.)
(leaf colorful-mode
  :elpaca t
  :hook ((prog-mode-hook . colorful-mode)
		 (css-mode-hook . colorful-mode)
		 (html-mode-hook . colorful-mode)))

;; Breadcrumb navigation (show file > class > method in headerline)
(leaf breadcrumb
  :elpaca t
  :hook (prog-mode-hook . breadcrumb-local-mode))

;; Quick-peek inline definitions without leaving current buffer
(leaf quick-peek
  :elpaca t)

;; Better xref navigation
(leaf xref
  :custom
  (xref-show-definitions-function . #'xref-show-definitions-completing-read)
  (xref-show-xrefs-function . #'xref-show-definitions-completing-read)
  (xref-search-program . 'ripgrep)
  :bind (("M-." . xref-find-definitions)
		 ("M-?" . xref-find-references)
		 ("M-," . xref-go-back)))

;; Automatically clean up trailing whitespace on save (only on changed lines)
(leaf ws-butler
  :elpaca t
  :hook (prog-mode-hook . ws-butler-mode))

;; Visualize and navigate errors with consult
(leaf consult-lsp
  :after (lsp-mode consult)
  :elpaca t
  :bind (:lsp-mode-map
		 ("C-c l d" . consult-lsp-diagnostics)
		 ("C-c l s" . consult-lsp-symbols)
		 ("C-c l f" . consult-lsp-file-symbols)))

;; Tree-sitter powered code folding
(leaf treesit-fold
  :elpaca (treesit-fold :host github :repo "emacs-tree-sitter/treesit-fold")
  :hook (prog-mode-hook . treesit-fold-mode))

;; Smart hungry delete - delete whitespace intelligently
(leaf hungry-delete
  :elpaca t
  :config
  (global-hungry-delete-mode 1)
  :custom
  (hungry-delete-chars-to-skip . " \t\f\v"))

;; Language-specific tree-sitter queries via consult
(leaf consult-imenu
  :bind (("C-c i" . consult-imenu)
		 ("C-c I" . consult-imenu-multi)))

(provide 'k-programming)
