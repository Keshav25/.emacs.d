;; Load Module Path
(add-to-list 'load-path "~/.emacs.d/modules")

;; Garbage Collection
(setq gc-cons-threshold most-positive-fixnum
	  gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
		  (lambda ()
			(message "*** Emacs loaded in %s with %d garbage collections."
					 (format "%.2f seconds"
							 (float-time
							  (time-subtract after-init-time before-init-time)))
					 gcs-done)))

;; Native Comp Errors
(setq comp-deferred-compilation nil
	  native-comp-deferred-compilation nil
	  comp-async-report-warnings-errors nil)

(setq load-prefer-newer noninteractive)

(setq package-enable-at-startup nil)

;; User Information
(setq user-full-name "Keshav Italia"
	  user-mail-address "keshavitalia0@gmail.com")

;; Language Environment
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

;; gobal modes
(global-so-long-mode 1)
(global-hl-line-mode -1)

;; Tab Width
(setq-default tab-width 4)

;; Advice Warnings
(setq ad-redefinition-action 'accept)

;; Ignore Startup Echo Message
(advice-add #'display-startup-echo-area-message :override #'ignore)

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
				  (eq window-system 'x)
				  (seq-contains command-line-args "--use-exwm")))

;; Bidirectional Text
(setq-default bidi-display-reordering 'left-to-right
			  bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;; Leaf and Repositories
(eval-and-compile
  (customize-set-variable
   'package-archives '(("melpa" . "https://melpa.org/packages/")
					   ("gnu" . "https://elpa.gnu.org/packages/")
					   ("elpa" . "https://elpa.gnu.org/packages/")))

  (when istermux
	(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))
  
  (require 'package)
  
  (unless (or (package-installed-p 'leaf) isguix)
	(package-refresh-contents)
	(package-install 'leaf))

  (leaf leaf-keywords
	:init
	(leaf hydra)
	(leaf el-get)
	(leaf blackout)))

(unless isguix
  (setq leaf-defaults (leaf-append-defaults '(:ensure t))))

;; leaf-convert
(leaf leaf-convert)

;; emacs-client
(leaf server
  :require t
  :defun server-running-p
  :config
  (unless (server-running-p) (server-start)))

;; macrostep
(leaf macrostep
  :ensure t
  :bind (("C-c m e" . macrostep-expand)
		 ("C-c m c" . macrostep-collapse)))

;; Auto Package Update
(leaf auto-package-update
  :setq
  (auto-package-update-delete-old-versions . t)
  (auto-package-update-hide-results . t)
  :config (auto-package-update-maybe))

;; Async
(leaf async
  :init
  (dired-async-mode 1))

;; Keep .emacs.d clean
(setq user-emacs-directory (expand-file-name "~/.cache/emacs")
	  url-history-file (expand-file-name "url/history" user-emacs-directory))

(leaf no-littering)

(setq custom-file
	  (if (boundp 'server-socket-dir)
		  (expand-file-name "custom.el" server-socket-dir)
		(expand-file-name (format "emacs-custom-$s.el" (user-uid)) temporary-file-directory)))

;; Gcmh
(leaf gcmh
  :config
  (gcmh-mode 1))

;; Visual Settings
(setq frame-resize-pixelwise t)
(setq window-resize-pixelwise nil)
(setq indicate-buffer-boundaries nil
	  indicate-empty-lines nil)

;; Disable Graphical Menus
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq menu-bar-mode nil
	  tool-bar-mode nil
	  scroll-bar-mode nil)

(setq initial-frame-alist
	  '((right-divider-width . 24)
		(alpha . (85 . 75))
		(internal-border-width. 24)))

;; Recursive Minibuffers
(setq enable-recursive-minibuffers t)

;; All the icons
(leaf all-the-icons)

;; hl-line-mode
(global-hl-line-mode -1)

;; Transparency
(set-frame-parameter (selected-frame) 'alpha `(85,50))
(add-to-list 'default-frame-alist `(alpha . (85, 50)))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Line Numbers
(leaf display-line-numbers
  :global-minor-mode global-display-line-numbers-mode
  :setq
  (display-line-numbers-type . 'relative)
  (display-line-numbers-width . 3))

;; (setq display-line-numbers-type t)
;; (add-hook 'prog-mode-hook 'display-line-numbers-mode)
;; (add-hook 'text-mode-hook 'display-line-numbers-mode)
;; (setq display-line-numbers-width 3)

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

(when (not istermux) (leaf solaire-mode))

;; Split and Follow Functions
(defun split-and-follow-horizontally ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)

(defun split-and-follow-vertically ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)

;;; http://blogs.fluidinfo.com/terry/2011/11/10/emacs-buffer-mode-histogram/
(defun buffer-mode-histogram ()
  "Display a histogram of emacs buffer modes."
  (interactive)
  (let* ((totals ())
		 (buffers (buffer-list()))
		 (total-buffers (length buffers))
		 (ht (make-hash-table :test 'equal)))
	(save-excursion
	  (dolist (buffer buffers)
		(set-buffer buffer)
		(let
			((mode-name (symbol-name major-mode)))
		  (puthash mode-name (1+ (gethash mode-name ht 0)) ht))))
	(maphash (lambda (key value)
			   (setq totals (cons (list key value) totals)))
			 ht)
	(setq totals (sort totals (lambda (x y) (> (cadr x) (cadr y)))))
	(with-output-to-temp-buffer "Buffer mode histogram"
	  (princ (format "%d buffers open, in %d distinct modes\n\n"
					 total-buffers (length totals)))
	  (dolist (item totals)
		(let
			((key (car item))
			 (count (cadr item)))
		  (if (equal (substring key -5) "-mode")
			  (setq key (substring key 0 -5)))
		  (princ (format "%2d %20s %s\n" count key
						 (make-string count ?+))))))))

;; Themes
(require 'k-themes)

;; Window-Management
(require 'k-wm)

;; Completion Framework
(require 'k-emocs)

;; Diminish
(leaf diminish)

;; Which-Key
(leaf which-key
  :init
  (which-key-mode))

;; Avy
(leaf avy)

;; Evil
(require 'k-evil)

;; General
(require 'k-general)

;; Dired
(require 'k-dired)

;; Org-Mode
(require 'k-org)

;; Elfeed
(require 'k-elfeed)

;; Vterm
(leaf vterm)

;; EXWM
(when isexwm
  (require 'k-exwm))

;; TEL
(leaf TEL
  :when istermux
  :doc "setup for my TEL configuration"
  :setq
  (inhibit-startup-screen . t)
  (vterm)
  (evil-emacs-state))


;; Experiment Section

(recentf-mode 1)

(leaf citar)
(leaf citar-embark)
