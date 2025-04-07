;; Doom Emacs Performance Improvements
(defvar last-file-name-handler-alist file-name-handler-alist)
(setq gc-cons-threshold most-positive-fixnum
	  gc-cons-percentage 0.6
	  file-name-handler-alist nil)

;; Restore to normal value after startup (e.g. 50MB)
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 50 1024 1024)
					  gc-cons-percentage 0.1
					  file-name-handler-alist last-file-name-handler-alist)))


;; User Information
(setq user-full-name "Keshav Italia"
	  user-mail-address "keshavitalia0@gmail.com")

(setq package-enable-at-startup nil
	  package-native-compile t
	  native-comp-always-compile t
	  native-comp-async-jobs-number (- (string-to-number (string-trim-right (shell-command-to-string "nproc"))) 1)
	  native-comp-async-report-warnings-errors 'silent
	  window-resize-pixelwise nil
	  frame-resize-pixelwise t
	  frame-inhibit-implied-resize t
	  inhibit-startup-screen t
	  warning-minimum-level :error
	  byte-compile-ignore-files t
	  load-prefer-newer t
	  auto-mode-case-fold nil
	  bidi-display-reordering 'left-to-right
      bidi-paragraph-direction 'left-to-right
	  bidi-inhibit-bpa t
	  idle-update-delay 1.0
	  inhibit-compacting-font-caches t
	  redisplay-skip-fontification-on-input t
	  frame-title-format `(,user-full-name " %b")
	  indicate-buffer-boundaries nil
	  indicate-empty-lines nil
	  ring-bell-function 'ignore
	  inhibit-splash-screen t)

(set-language-environment "UTF-8")
(setq default-input-method nil)

;; Disable Graphical Menus
(setq-default default-frame-alist
			  '((tool-bar-lines . 0)
				(menu-bar-lines . 0)
				(alpha-background . 90)
				(alpha . 100)
				;; (undecorated . t)
				(vertical-scroll-bars . nil)
				(horizontal-scroll-bars . nil)
				(font . "JetBrainsMono-12:weigth=regular:width=normal")))

(setq menu-bar-mode nil
	  tool-bar-mode nil
	  scroll-bar-mode nil)
