;; -*- lexical-binding: t -*-

;; init benchmarking
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

(require 'server)
(unless (server-running-p)
		(server-start))

;; Process performance tuning
(setq read-process-output-max (* 4 1024 1024))
(setq process-adaptive-read-buffering nil)

;; startup message
;; (add-hook 'emacs-startup-hook
;; 		  (lambda ()
;; 			(message "*** Emacs loaded in %s with %d garbage collections."
;; 					 (format "%.2f seconds"
;; 							 (float-time
;; 							  (time-subtract after-init-time before-init-time)))
;; 					 gcs-done)))

;; Native Comp Errors
(setq
 comp-deferred-compilation nil
 native-comp-jit-compilation t
 comp-async-report-warnings-errors nil
 package-install-upgrade-built-in t)

(setq warning-suppress-log-types '((package reinitialization (comp) (bytecomp))))

(setq vc-handled-backends '(Git))

;; refresh buffer on file change
(global-auto-revert-mode t)

;; Because Windows
(setq gnutls-algorithm-priority "SECURE128:+SECURE192:-VERS-ALL:+VERS-TLS1.2:+VERS-TLS1.3")  

;; backup files
(setq backup-directory-alist
	  `(("."
		 .
		 ,(expand-file-name "tmp/backups/" user-emacs-directory)))
	  create-lockfiles nil)

(make-directory (expand-file-name "tmp/auto-saves/"
								  user-emacs-directory)
								  t)
(setq
 auto-save-list-file-prefix
 (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
 auto-save-file-name-transforms
 `((".*"
	,(expand-file-name "tmp/auto-saves/" user-emacs-directory)
	t)))

(setq lsp-session-file
	  (expand-file-name "tmp/.lsp-session-v1" user-emacs-directory))

(setq
 backup-by-copying t ; don't clobber symlinks
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t) ; use versioned backups

(message "Deleting old backup files...")
(let ((week (* 60 60 24 7))
	  (current (float-time (current-time))))
	  (dolist (file (directory-files temporary-file-directory t))
		(when (and (backup-file-name-p file)
				   (> (- current
						 (float-time (fifth (file-attributes file))))
					  (* 4 week)))
		  (message "%s" file)
		  (delete-file file))))


;; recentf-mode
(recentf-mode 1)

;; Determine OS
(require 'subr-x)
(defconst islinux (eq system-type 'gnu/linux))
(defconst iswindows (eq system-type 'windows-nt))
(defconst istermux
  (string-suffix-p
   "Android" (string-trim (shell-command-to-string "uname -a"))))
(defconst isguix nil)

;; Get Home Directory if Windows
(when (and iswindows (null (getenv-internal "HOME")))
	  (setenv "HOME" (getenv "USERPROFILE"))
	  (setq abbreviated-home-dir nil))

;; Determine if Native Comp
(defconst isnativecomp
  (if (fboundp 'native-comp-available-p)
	(native-comp-available-p)))

;; Determine if EXWM should be enabled
(setq isexwm (and (not istermux) (eq window-system 'x)))

;; Language Environment
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

;; global modes
(global-so-long-mode 1)

;; Tab Width
(setq-default tab-width 4)

;; Advice Warnings
(setq ad-redefinition-action 'accept)

;; Ignore Startup Echo Message
(advice-add #'display-startup-echo-area-message :override #'ignore)

;; Bidirectional Text
(setq-default
 bidi-display-reordering 'left-to-right
 bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)


;; Keep .emacs.d clean
(setq
 user-emacs-directory (expand-file-name "~/.emacs.d")
 url-history-file (expand-file-name "url/history" user-emacs-directory))


(setq custom-file
      (if (boundp 'server-socket-dir)
        (expand-file-name "~/.emacs.d/custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-$s.el" (user-uid))
                          temporary-file-directory)))


(setq initial-frame-alist
      '((right-divider-width . 24) (internal-border-width. 24)))

;; Recursive Minibuffers
(setq enable-recursive-minibuffers t)

;; prevent restoring windows after exiting the minibuffer
(setq read-minibuffer-restore-windows nil)

(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))


;; Line Numbers
(setq display-line-numbers-type t)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)
(setq display-line-numbers-type 'relative)
(setq display-line-numbers-width 3)

;; General Text Editing Preferences
(show-paren-mode)
(global-prettify-symbols-mode t)
(setq electric-pair-pairs
      '((?\{ . ?\}) (?\( . ?\)) (?\[ . ?\]) (?\" . ?\")))
(electric-pair-mode t)

;; Display Time
(display-time-mode 1)
(setq display-time-day-and-date 1)

;; Cursor Settings
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)
(setq fast-but-imprecise-scrolling t)
(setq
 hscroll-margin 2
 hscroll-step 1
 scroll-conservatively 101
 scroll-margin 0
 scroll-preserve-screen-position t
 auto-window-vscroll nil
 mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
 mouse-wheel-scroll-amount-horizon 2)
(setq x-stretch-cursor nil)

(delete-selection-mode +1)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq use-short-answers t)
(setq sentence-end-double-space nil)
(setq  save-interprogram-paste-before-kill t)
(setq truncate-string-ellipsis "...")
(setq eval-expression-print-length nil
      eval-expression-print-level nil)
(setq x-underline-at-descent-line t)

;; Delete by moving to trash in interactive mode
(setq delete-by-moving-to-trash (not noninteractive))
(setq remote-file-name-inhibit-delete-by-moving-to-trash t)

;; Ignoring this is acceptable since it will redirect to the buffer regardless.
(setq find-file-suppress-same-file-warnings t)

;; Resolve symlinks so that operations are conducted from the file's directory
(setq find-file-visit-truename t
      vc-follow-symlinks t)

(setq confirm-nonexistent-file-or-buffer nil)

;;; Auto revert
;; Auto-revert in Emacs is a feature that automatically updates the contents of
;; a buffer to reflect changes made to the underlying file.
(setq revert-without-query (list ".")  ; Do not prompt
      auto-revert-stop-on-user-input nil
      auto-revert-verbose t)

;; Revert other buffers (e.g, Dired)
(setq global-auto-revert-non-file-buffers t)
(setq global-auto-revert-ignore-modes '(Buffer-menu-mode))

(setq-default word-wrap t)
;; load libraries
;; (load-library "f")

(load-file "~/.emacs.d/config.el")

(setopt gc-cons-threshold 100000000)

(provide 'init)
