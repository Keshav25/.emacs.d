;; -*- lexical-binding: t -*-

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

(defgroup golang nil
  "golang group."
  :prefix "golang-"
  :group 'tools)

(defcustom golang-mode-keymap-prefix (kbd "C-c C-n")
  "Golang minor mode keymap prefix."
  :group 'golang
  :type 'string)


;;;###autoload
(defun golang-mod-tidy ()
  (interactive)
  (golang-command "go mod tidy"))

;;;###autoload
(defun golang-add-package (package-name)
  "Add package reference from PACKAGE-NAME."
  (interactive "sPackage name: ")
  (golang-command (concat "go get " package-name)))

;;;###autoload
(defun golang-build ()
  "Build a .NET project."
  (interactive)
  (let* ((target (golang-select-project-or-solution))
         (command "go build \"%s\""))
    (compile (format command target))))

;;;###autoload
(defun golang-clean ()
  "Clean build output."
  (interactive)
  (golang-command "golang clean -v n"))

(defvar golang-langs '("c#" "f#"))
(defvar golang-templates '("console" "classlib" "mstest" "xunit" "web" "mvc" "webapi"))

(defvar golang-compilation-regexps
  (cons
   "^\\([^\n]+\\)(\\([0-9]+\\),\\([0-9]+\\)): \\(?:error\\|\\(warning\\)\\)"
   '(1 2 3 (4 . 5) 1)))

;;;###autoload
(defun golang-new (project-path name)
  "Initialize a new Go project."
  (interactive (list (read-directory-name "Project path: ")
                     (read-string "Name: ")))
  (let ((default-directory project-path))
    (shell-command (concat "go mod init " name))))

;;;###autoload
(defun golang-restore ()
  "Restore dependencies specified in the .NET project."
  (interactive)
  (golang-command "golang restore"))

(defvar golang-run-last-proj-dir nil
  "Last project directory executed by `golang-run'.")

;;;###autoload
(defun golang-run (arg)
  "Compile and execute a .NET project.  With ARG, query for project path again."
  (interactive "P")
  (when (or (not golang-run-last-proj-dir) arg)
    (setq golang-run-last-proj-dir (read-directory-name "Run project in directory: ")))
  (let ((default-directory golang-run-last-proj-dir))
    (detached-compile (concat "go run " golang-run-last-proj-dir))))

;;;###autoload
(defun golang-run-with-args (args)
  "Compile and execute a .NET project with ARGS."
  (interactive "Arguments: ")
  (golang-command (concat "golang run " args)))

;;;###autoload
(defun golang-sln-add ()
  "Add a project to a Solution."
  (interactive)
  (let ((solution-file (read-file-name "Solution file: ")))
    (let ((to-add (read-file-name "Project/Pattern to add to the solution: ")))
      (golang-command (concat "golang sln " solution-file " add " to-add)))))

;;;###autoload
(defun golang-sln-remove ()
  "Remove a project from a Solution."
  (interactive)
  (let ((solution-file (read-file-name "Solution file: ")))
    (let ((to-remove (read-file-name "Project/Pattern to remove from the solution: ")))
      (golang-command (concat "golang sln " solution-file " remove " to-remove)))))

;;;###autoload
(defun golang-sln-list ()
  "List all projects in a Solution."
  (interactive)
  (let ((solution-file (read-file-name "Solution file: ")))
    (golang-command (concat "golang sln " solution-file " list"))))

;;;###autoload
(defun golang-sln-new ()
  "Create a new Solution."
  (interactive)
  (let ((solution-path (read-directory-name "Solution path: ")))
    (golang-command (concat "golang new sln -o " solution-path))))

(defvar golang-test-last-test-proj nil
  "Last unit test project file executed by `golang-test'.")

;;;###autoload
(defun golang-test (arg)
  "Launch project unit-tests, querying for a project on first call.  With ARG, query for project path again."
  (interactive "P")
  (when (or (not golang-test-last-test-proj) arg)
    (setq golang-test-last-test-proj (read-file-name "Launch tests for Project file: ")))
  (golang-command (concat "golang test " golang-test-last-test-proj)))

(defun golang-command (cmd)
  "Run CMD in an async buffer."
  (detached-compile cmd))

(defun golang-find (extension)
  "Search for a EXTENSION file in any enclosing folders relative to current directory."
  (golang-search-upwards (rx-to-string extension)
                         (file-name-directory buffer-file-name)))

(defun golang-goto (extension)
  "Open file with EXTENSION in any enclosing folders relative to current directory."
  (let ((file (golang-find extension)))
    (if file
        (find-file file)
      (error "Could not find any %s file" extension))))

(defun golang-goto-sln ()
  "Search for a solution file in any enclosing folders relative to current directory."
  (interactive)
  (golang-goto ".sln"))

(defun golang-goto-csproj ()
  "Search for a C# project file in any enclosing folders relative to current directory."
  (interactive)
  (golang-goto ".csproj"))

(defun golang-goto-fsproj ()
  "Search for a F# project file in any enclosing folders relative to current directory."
  (interactive)
  (golang-goto ".fsproj"))

(defun golang-search-upwards (regex dir)
  "Search for REGEX in DIR."
  (when dir
    (or (car-safe (directory-files dir 'full regex))
        (golang-search-upwards regex (golang-parent-dir dir)))))

(defun golang-parent-dir (dir)
  "Find parent DIR."
  (let ((p (file-name-directory (directory-file-name dir))))
    (unless (equal p dir)
      p)))

(defun golang-select-project-or-solution ()
  "Prompt for the project/solution file or directory.  Try projectile root first, else use current buffer's directory."
  (let ((default-dir-prompt "?"))
    (ignore-errors
      (when (fboundp 'projectile-project-root)
        (setq default-dir-prompt (projectile-project-root))))
    (when (string= default-dir-prompt "?")
      (setq default-dir-prompt default-directory))
    (expand-file-name (read-file-name "Project or solution: " default-dir-prompt nil t))))

(defun golang-valid-project-solutions (path)
  "Predicate to validate project/solution paths.  PATH is passed by `'read-file-name`."
  ;; file-attributes returns t for directories
  ;; if not a dir, then check the common extensions
  (let ((extension (file-name-extension path))
        (valid-projects (list "sln" "csproj" "fsproj")))
    (or (member extension valid-projects)
        (car (file-attributes path)))))

(defvar golang-mode-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a p") #'golang-add-package)
    (define-key map (kbd "a r") #'golang-add-reference)
    (define-key map (kbd "b")   #'golang-build)
    (define-key map (kbd "c")   #'golang-mod-tidy)
    (define-key map (kbd "g c") #'golang-goto-csproj)
    (define-key map (kbd "g f") #'golang-goto-fsproj)
    (define-key map (kbd "g s") #'golang-goto-sln)
    (define-key map (kbd "n")   #'golang-new)
    (define-key map (kbd "p")   #'golang-publish)
    (define-key map (kbd "r")   #'golang-restore)
    (define-key map (kbd "e")   #'golang-run)
    (define-key map (kbd "C-e") #'golang-run-with-args)
    (define-key map (kbd "s a") #'golang-sln-add)
    (define-key map (kbd "s l") #'golang-sln-list)
    (define-key map (kbd "s n") #'golang-sln-new)
    (define-key map (kbd "s r") #'golang-sln-remove)
    (define-key map (kbd "t")   #'golang-test)
    map)
  "Keymap for golang-mode commands after `golang-mode-keymap-prefix'.")

(defvar golang-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd golang-mode-keymap-prefix) golang-mode-command-map)
    map)
  "Keymap for golang-mode.")

;;;###autoload
(define-minor-mode golang-mode
  "golang CLI minor mode."
  nil
  " golang"
  golang-mode-map
  :group 'golang
  ;; (add-to-list 'compilation-error-regexp-alist-alist
  ;; (cons 'golang golang-compilation-regexps))
  ;; (add-to-list 'compilation-error-regexp-alist 'golang))
  )

(provide 'k-go)
