;; -*- lexical-binding: t -*-

;;; k-go.el --- Go language configuration  -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for Go development with LSP, DAP, tree-sitter and common tooling.

;;; Code:

(leaf lsp-go
  :after (lsp-mode)
  :custom
  (lsp-go-use-gofumpt . t)
  (lsp-go-analyses . '((unusedparams . t)
					   (shadow . t)
					   (unusedwrite . t)
					   (nilness . t)
					   (useany . t)
					   (fieldalignment . t))))

(leaf dap-go
  :after (dap)
  :config
  (require 'dap-dlv-go))

(leaf go-mode
  :elpaca t
  :hook ((go-mode-hook . lsp-deferred)
		 (go-mode-hook . subword-mode)
		 (before-save-hook . (lambda ()
							   (when (derived-mode-p 'go-mode 'go-ts-mode)
								 (lsp-organize-imports)
								 (lsp-format-buffer))))))

(leaf go-ts-mode
  :hook ((go-ts-mode-hook . lsp-deferred)
		 (go-ts-mode-hook . subword-mode)))

(leaf templ-ts-mode
  :elpaca t)

(leaf gotest
  :elpaca t
  :after go-mode
  :bind (:go-mode-map
		 ("C-c t t" . go-test-current-test)
		 ("C-c t f" . go-test-current-file)
		 ("C-c t p" . go-test-current-project)
		 ("C-c t b" . go-test-current-benchmark)
		 ("C-c t c" . go-test-current-coverage)
		 ("C-c t r" . go-run)))

;; ============================================================
;; Go project utilities
;; ============================================================

(defgroup k-go nil
  "Go development utilities."
  :prefix "k-go-"
  :group 'tools)

(defun k-go-mod-tidy ()
  "Run `go mod tidy' in the project root."
  (interactive)
  (let ((default-directory (or (locate-dominating-file default-directory "go.mod")
							   default-directory)))
	(compile "go mod tidy")))

(defun k-go-add-package (package-name)
  "Add Go package PACKAGE-NAME via `go get'."
  (interactive "sPackage name: ")
  (let ((default-directory (or (locate-dominating-file default-directory "go.mod")
							   default-directory)))
	(compile (concat "go get " package-name))))

(defun k-go-build ()
  "Build the current Go project."
  (interactive)
  (let ((default-directory (or (locate-dominating-file default-directory "go.mod")
							   default-directory)))
	(compile "go build ./...")))

(defun k-go-run ()
  "Run the current Go file or project."
  (interactive)
  (if buffer-file-name
	  (compile (concat "go run " (shell-quote-argument buffer-file-name)))
	(let ((default-directory (or (locate-dominating-file default-directory "go.mod")
								 default-directory)))
	  (compile "go run ."))))

(defun k-go-test ()
  "Run all tests in the current Go project."
  (interactive)
  (let ((default-directory (or (locate-dominating-file default-directory "go.mod")
							   default-directory)))
	(compile "go test -v ./...")))

(defun k-go-test-current ()
  "Run the test function at point."
  (interactive)
  (let* ((test-name (save-excursion
					  (re-search-backward "^func \\(Test[A-Za-z0-9_]+\\)" nil t)
					  (match-string 1)))
		 (default-directory (file-name-directory (buffer-file-name))))
	(if test-name
		(compile (format "go test -v -run %s ." test-name))
	  (message "No test function found at or above point"))))

(defun k-go-vet ()
  "Run `go vet' on the current project."
  (interactive)
  (let ((default-directory (or (locate-dominating-file default-directory "go.mod")
							   default-directory)))
	(compile "go vet ./...")))

(defun k-go-mod-init (module-name)
  "Initialize a new Go module with MODULE-NAME."
  (interactive "sModule name: ")
  (compile (concat "go mod init " module-name)))

(defun k-go-goto-mod ()
  "Open the go.mod file for the current project."
  (interactive)
  (let ((mod-file (locate-dominating-file default-directory "go.mod")))
	(if mod-file
		(find-file (expand-file-name "go.mod" mod-file))
	  (message "No go.mod found in any parent directory"))))

(defvar k-go-command-map
  (let ((map (make-sparse-keymap)))
	(define-key map (kbd "b")   #'k-go-build)
	(define-key map (kbd "r")   #'k-go-run)
	(define-key map (kbd "t")   #'k-go-test)
	(define-key map (kbd "T")   #'k-go-test-current)
	(define-key map (kbd "v")   #'k-go-vet)
	(define-key map (kbd "a")   #'k-go-add-package)
	(define-key map (kbd "m")   #'k-go-mod-tidy)
	(define-key map (kbd "i")   #'k-go-mod-init)
	(define-key map (kbd "g")   #'k-go-goto-mod)
	map)
  "Keymap for Go commands, accessed via `C-c G'.")

(define-key go-mode-map (kbd "C-c G") k-go-command-map)

(provide 'k-go)
