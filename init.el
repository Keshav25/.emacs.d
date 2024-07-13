;; emacsclient
(require 'server)
(unless (server-running-p)
  (server-start))

;; Garbage Collection
(setq
 gc-cons-threshold most-positive-fixnum
 gc-cons-percentage 0.6)

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
 native-comp-jit-compilation nil
 comp-async-report-warnings-errors nil
 package-install-upgrade-built-in t)

(setq load-prefer-newer noninteractive)

(setq package-enable-at-startup nil)
(setq package-native-compile t)

(setq warning-suppress-log-types '((package reinitialization)))

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

(setq
 projectile-known-projects-file
 (expand-file-name "tmp/projectile-bookmarks.eld"
				   user-emacs-directory)
 lsp-session-file (expand-file-name "tmp/.lsp-session-v1" user-emacs-directory))

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

;; Toggle Word Wrap
(visual-line-mode 1)

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


;; Visual Settings
(setq frame-resize-pixelwise t)
(setq window-resize-pixelwise nil)
(setq
 indicate-buffer-boundaries nil
 indicate-empty-lines nil)

;; Disable Graphical Menus
(setq-default default-frame-alist
              '((tool-bar-lines . 0)
                (menu-bar-lines . 0)
				(alpha-background . 65)
                ;; (undecorated . t)
                (vertical-scroll-bars . nil)
                (horizontal-scroll-bars . nil)
				(font . "JetBrainsMono-12:weigth=regular:width=normal")))

;; (push '(menu-bar-lines . 0)   default-frame-alist)
;; (push '(tool-bar-lines . 0)   default-frame-alist)
;; (push '(vertical-scroll-bars) default-frame-alist)

;; (setq menu-bar-mode nil
;; tool-bar-mode nil
;; scroll-bar-mode nil)

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
(setq display-time-mode 1)
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

;; load libraries
;; (load-library "f")

(load-file "~/.emacs.d/config.el")
(provide 'init)
