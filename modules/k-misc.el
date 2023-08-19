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
;;          ([remap detached-open-session] . detached-consult-session))
;;   :custom ((detached-show-output-on-attach t)
;;            (detached-terminal-data-command system-type)))

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

(leaf mastodon
  :ensure t
  :require t
  :config
  (mastodon-discover))

(leaf moldable-emacs
  :init (if (f-directory-p "~/.emacs.d/site-lisp/moldable-emacs")
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

(provide 'k-misc)
