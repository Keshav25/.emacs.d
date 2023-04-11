;; emacsclient
(require 'server)
(unless (server-running-p)
  (server-start))

;; Garbage Collection
(setq gc-cons-threshold most-positive-fixnum
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
(setq comp-deferred-compilation nil
	  native-comp-deferred-compilation nil
	  comp-async-report-warnings-errors nil)

(setq load-prefer-newer noninteractive)

(setq package-enable-at-startup nil)
(setq package-native-compile t)

;; backup files
(setq backup-directory-alist '(("." . "~/.saves")))
(setq auto-save-file-name-transformations '(("." . "~/.saves")))
(setq backup-by-copying t      ; don't clobber symlinks
	  delete-old-versions t
	  kept-new-versions 6
	  kept-old-versions 2
	  version-control t)       ; use versioned backups

(message "Deleting old backup files...")
(let ((week (* 60 60 24 7))
      (current (float-time (current-time))))
  (dolist (file (directory-files temporary-file-directory t))
    (when (and (backup-file-name-p file)
               (> (- current (float-time (fifth (file-attributes file))))
                  (* 4 week)))
      (message "%s" file)
      (delete-file file))))


;; recentf-mode
(recentf-mode 1)


;; Determine OS
(require 'subr-x)
(defconst islinux (eq system-type 'gnu/linux))
(defconst iswindows (eq system-type '(cygwin windows-nt ms-dos)))
(defconst istermux (string-suffix-p "Android" (string-trim (shell-command-to-string "uname -a"))))
(defconst isguix nil)

;; Get Home Directory if Windows
(when (and iswindows (null (getenv-internal "HOME")))
  (setenv "HOME" (getenv "USERPROFILE"))
  (setq abbreviated-home-dir nil))

;; Determine if Native Comp
(defconst isnativecomp (if (fboundp 'native-comp-available-p)
						   (native-comp-available-p)))

;; Determine if EXWM should be enabled
(setq isexwm (and (not istermux)
				  (eq window-system 'x)))

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
(setq-default bidi-display-reordering 'left-to-right
			  bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)


;; Keep .emacs.d clean
(setq user-emacs-directory (expand-file-name "~/.cache/emacs")
	  url-history-file (expand-file-name "url/history" user-emacs-directory))



(setq custom-file
	  (if (boundp 'server-socket-dir)
		  (expand-file-name "~/.emacs.d/custom.el" server-socket-dir)
		(expand-file-name (format "emacs-custom-$s.el" (user-uid)) temporary-file-directory)))


;; Visual Settings
(setq frame-resize-pixelwise t)
(setq window-resize-pixelwise nil)
(setq indicate-buffer-boundaries nil
	  indicate-empty-lines nil)

;; Disable Graphical Menus
(setq-default default-frame-alist
			  '((tool-bar-lines . 0)
				(menu-bar-lines . 0)
				(undecorated . t)
				(vertical-scroll-bars . nil)
				(horizontal-scroll-bars . nil)))
;; (push '(menu-bar-lines . 0)   default-frame-alist)
;; (push '(tool-bar-lines . 0)   default-frame-alist)
;; (push '(vertical-scroll-bars) default-frame-alist)

;; (setq menu-bar-mode nil
	  ;; tool-bar-mode nil
	  ;; scroll-bar-mode nil)

(setq initial-frame-alist
	  '((right-divider-width . 24)
		(internal-border-width. 24)))

;; Recursive Minibuffers
(setq enable-recursive-minibuffers t)


;; hl-line-mode
(global-hl-line-mode -1)

;; Transparency
(if (or (< emacs-major-version 29) t) ;;not yet
	(progn
	  (set-frame-parameter (selected-frame) 'alpha `(95,95))
	  (add-to-list 'default-frame-alist `(alpha . (95, 95)))

	  (defun turn-transparency-off ()
		(interactive)
		(set-frame-parameter (selected-frame) 'alpha `(100, 100)))

	  (defun turn-transparency-on ()
		(interactive)
		(set-frame-parameter (selected-frame) 'alpha `(95, 95))))
  (progn
	;; for emacs => 29
	(set-frame-parameter (selected-frame) 'alpha-background 75)
	(add-to-list 'default-frame-alist '(alpha-background . 75))

	(defun k/toggle-transparency ()
	  "toggle transparency for emacs-major-version > 29"
	  (interactive)
	  (let ((alpha-transparency 75))
		(pcase (frame-parameter nil 'alpha-background)
		  (alpha-transparency (set-frame-parameter nil 'alpha-background 100))
		  (t (set-frame-parameter nil 'alpha-background alpha-transparency)))))))


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
(setq electric-pair-pairs '((?\{ . ?\})
							(?\( . ?\))
							(?\[ . ?\])
							(?\" . ?\")
							))
(electric-pair-mode t)

;; Display Time
(setq display-time-mode 1)
(setq display-time-day-and-date 1)

;; Cursor Settings
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)
(setq fast-but-imprecise-scrolling t)
(setq hscroll-margin 2
	  hscroll-step 1
	  scroll-conservatively 101
	  scroll-margin 0
	  scroll-preserve-screen-position t
	  auto-window-vscroll nil
	  mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
	  mouse-wheel-scroll-amount-horizon 2)
(setq x-stretch-cursor nil)

(provide 'early-init)
