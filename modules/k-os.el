;; For any packages that integrate with an OS

(leaf alert
  :ensure t
  :require t
  :config
  (if istermux
	  (setq alert-default-style 'termux)
	(setq alert-default-style 'libnotify))
  (when (eq system-type 'windows-nt)
    (alert-define-style
     'windows-desktop-notification-style
     :title "Windows Desktop Notification style"
     :notifier
     (lambda (info)
       (let ((notif-id (w32-notification-notify :title (plist-get info :title) :body (plist-get info :message))))
         ;; Close it after 3 seconds (no new notification can be sent if left unclosed)
         (run-with-timer 3 nil `(lambda() (w32-notification-close ,notif-id))))))
    (setq alert-default-style 'windows-desktop-notification-style))
  :bind ((:log4e-mode-map
		  ("q" . kill-current-buffer))))

(leaf prodigy
  :ensure t)

(leaf edit-server
  :ensure t
  :commands edit-server-start
  :setq
  (edit-server-new-frame . nil)
  :config
  (if after-init-time
	  (edit-server-start)
	(add-hook 'after-init-hook
			  #'(lambda nil
				  (edit-server-start)))))

(leaf eaf
  :disabled t
  :load-path "~/.emacs.d/quelpa/build/eaf"
  :quelpa (eaf :fetcher github :repo "emacs-eaf/emacs-application-framework")
  :init
  (leaf epc :ensure t)
  (leaf ctable :ensure t)
  (leaf deferred :ensure t)
  (leaf s :ensure t)
  ;; (require 'eaf)
  :setq
  (browser-url-browser-function . 'eaf-open-browser)
  (eaf-browser-default-search-engine . "duckduckgo")
  (eaf-browser-keybinding . '(("C--" . "zoom_out")
							  ("C-=" . "zoom_in")
							  ("C-0" . "zoom_reset")
							  ("C-s" . "search_text_forward")
							  ("C-r" . "search_text_backward")
							  ("C-n" . "scroll_up")
							  ("C-p" . "scroll_down")
							  ("C-f" . "scroll_right")
							  ("C-b" . "scroll_left")
							  ("C-v" . "scroll_up_page")
							  ("C-y" . "yank_text")
							  ("C-w" . "kill_text")
							  ("M-e" . "atomic_edit")
							  ("M-c" . "caret_toggle_browsing")
							  ("M-D" . "select_text")
							  ("M-s" . "open_link")
							  ("M-S" . "open_link_new_buffer")
							  ("M-d" . "open_link_background_buffer")
							  ("C-/" . "undo_action")
							  ("M-_" . "redo_action")
							  ("M-w" . "copy_text")
							  ("M-f" . "history_forward")
							  ("M-b" . "history_backward")
							  ("M-q" . "clear_cookies")
							  ("C-t" . "toggle_password_autofill")
							  ("C-d" . "save_page_password")
							  ("M-a" . "toggle_adblocker")
							  ("C-M-q" . "clear_history")
							  ("C-M-i" . "import_chrome_history")
							  ("M-v" . "scroll_down_page")
							  ("M-<" . "scroll_to_begin")
							  ("M->" . "scroll_to_bottom")
							  ("M-p" . "duplicate_page")
							  ("M-t" . "new_blank_page")
							  ("<" . "insert_or_select_left_tab")
							  (">" . "insert_or_select_right_tab")
							  ("j" . "insert_or_scroll_up")
							  ("k" . "insert_or_scroll_down")
							  ("h" . "insert_or_scroll_left")
							  ("l" . "insert_or_scroll_right")
							  ("f" . "insert_or_open_link")
							  ("F" . "insert_or_open_link_new_buffer")
							  ("B" . "insert_or_open_link_background_buffer")
							  ("c" . "insert_or_caret_at_line")
							  ("J" . "insert_or_scroll_up_page")
							  ("K" . "insert_or_scroll_down_page")
							  ("H" . "insert_or_history_backward")
							  ("L" . "insert_or_history_forward")
							  ("t" . "insert_or_new_blank_page")
							  ("T" . "insert_or_recover_prev_close_page")
							  ("i" . "insert_or_focus_input")
							  ("I" . "insert_or_open_downloads_setting")
							  ("r" . "insert_or_refresh_page")
							  ("g" . "insert_or_scroll_to_begin")
							  ("x" . "insert_or_close_buffer")
							  ("G" . "insert_or_scroll_to_bottom")
							  ("-" . "insert_or_zoom_out")
							  ("=" . "insert_or_zoom_in")
							  ("0" . "insert_or_zoom_reset")
							  ("m" . "insert_or_save_as_bookmark")
							  ("o" . "insert_or_open_browser")
							  ;; ("y" . "insert_or_download_youtube_video")
							  ("y" . "insert_or_copy_text")
							  ("Y" . "insert_or_download_youtube_audio")
							  ("p" . "insert_or_toggle_device")
							  ("P" . "insert_or_duplicate_page")
							  ("1" . "insert_or_save_as_pdf")
							  ("2" . "insert_or_save_as_single_file")
							  ("v" . "insert_or_view_source")
							  ("e" . "insert_or_edit_url")
							  ("M-C" . "copy_code")
							  ("C-M-f" . "copy_link")
							  ("C-a" . "select_all_or_input_text")
							  ("M-u" . "clear_focus")
							  ("C-j" . "open_downloads_setting")
							  ("M-o" . "eval_js")
							  ("M-O" . "eval_js_file")
							  ("<escape>" . "eaf-browser-send-esc-or-exit-fullscreen")
							  ("M-," . "eaf-send-down-key")
							  ("M-." . "eaf-send-up-key")
							  ("M-m" . "eaf-send-return-key")
							  ("<f5>" . "refresh_page")
							  ("<f12>" . "open_devtools")
							  ("<C-return>" . "eaf-send-ctrl-return-sequence")))
  :config
  (require 'eaf-browser)
  (require 'eaf-terminal)
  (require 'eaf-org-previewer)
  (require 'eaf-mindmap)
  (require 'eaf-pdf-viewer)
  (require 'eaf-video-player)
  (require 'eaf-markdown-previewer)
  (require 'eaf-music-player)
  (require 'eaf-rss-reader)
  (require 'eaf-file-manager)
  (require 'eaf-jupyter)
  (require 'eaf-image-viewer)
  (require 'eaf-camera)
  (require 'eaf-system-monitor)
  (require 'eaf-file-browser)
  (require 'eaf-file-sender)
  (require 'eaf-airshare)
  (require 'eaf-netease-cloud-music))

;; TEL
(leaf TEL
  :when istermux
  :doc "setup for my TEL configuration"
  :setq
  (inhibit-startup-screen . t)
  :config
  (eshell))

(leaf windows
  :when iswindows
  :doc "the source of my anger issues"
  :config
  (defun wsl (command)
	"run shell-command in wsl"
	(interactive "sCommand: ")
	(async-shell-command (concat "wsl " command)))

  (defun wsl-path (path)
	"Convert a Windows path to a WSL-compatible path."
	(replace-regexp-in-string "C:" "/mnt/c"
							  (replace-regexp-in-string "\\\\" "/" path)))

  (defun run-in-wsl-dir (dir command)
	"Prompt for a directory DIR and a COMMAND to run in WSL."
	(interactive "DDirectory: 
sCommand: ")
	(wsl (format "cd %s && %s" (wsl-path dir) command)))

  (defun run-in-wsl-file (command)
	"Run a COMMAND in WSL on the current file."
	(interactive "sCommand: ")
	(let ((file (wsl-path buffer-file-name)))
      (wsl (format "%s %s" command file)))))

(leaf powershell
  :disabled (not iswindows)
  :ensure t)

(leaf spookfox
  :doc "only works on linux"
  :disabled t
  :quelpa (spookfox :fetcher github
					:repo "bitspook/spookfox"
					:files ("lisp/*.el" "lisp/apps/*.el"))
  :require t
  :config
  (setq spookfox-enabled-apps '(spookfox-org-tabs))
  (spookfox-init))

(leaf whicher
  :quelpa (whicher :fetcher github
				   :repo "abo-abo/whicher"
				   :files ("*.el")))

(leaf define-word
  :quelpa (define-word :fetcher github
					   :repo "abo-abo/define-word"
					   :files ("*.el")))

(leaf dmenu :ensure t)

(leaf sudo-edit
  :after (embark)
  :bind
  (:embark-file-map
   ("s". sudo-edit-find-file))
  (:embark-become-file+buffer-map
   ("s" . sudo-edit-find-file)))

(provide 'k-os)
