;; -*- lexical-binding: t -*-

(leaf lsp-go
  :after (lsp))

(leaf dap-go
  :after (dap)
  :config
  (require 'dap-dlv-go))

(defgroup golang nil
  "dotnet group."
  :prefix "dotnet-"
  :group 'tools)

(defcustom golang-mode-keymap-prefix (kbd "C-c C-n")
  "Dotnet minor mode keymap prefix."
  :group 'dotnet
  :type 'string)


;;;###autoload
(defun golang-mod-tidy ()
  (interactive)
  (golang-command "go mod tidy"))

;;;###autoload
(defun golang-add-package (package-name)
  "Add package reference from PACKAGE-NAME."
  (interactive "sPackage name: ")
  (dotnet-command (concat "go get " package-name)))

;;;###autoload
(defun golang-build ()
  "Build a .NET project."
  (interactive)
  (let* ((target (golang-select-project-or-solution))
         (command "go build \"%s\""))
    (compile (format command target))))

;;;###autoload
(defun dotnet-clean ()
  "Clean build output."
  (interactive)
  (dotnet-command "dotnet clean -v n"))

(defvar dotnet-langs '("c#" "f#"))
(defvar dotnet-templates '("console" "classlib" "mstest" "xunit" "web" "mvc" "webapi"))

(defvar dotnet-compilation-regexps
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
(defun dotnet-restore ()
  "Restore dependencies specified in the .NET project."
  (interactive)
  (dotnet-command "dotnet restore"))

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
(defun dotnet-run-with-args (args)
  "Compile and execute a .NET project with ARGS."
  (interactive "Arguments: ")
  (dotnet-command (concat "dotnet run " args)))

;;;###autoload
(defun dotnet-sln-add ()
  "Add a project to a Solution."
  (interactive)
  (let ((solution-file (read-file-name "Solution file: ")))
    (let ((to-add (read-file-name "Project/Pattern to add to the solution: ")))
      (dotnet-command (concat "dotnet sln " solution-file " add " to-add)))))

;;;###autoload
(defun dotnet-sln-remove ()
  "Remove a project from a Solution."
  (interactive)
  (let ((solution-file (read-file-name "Solution file: ")))
    (let ((to-remove (read-file-name "Project/Pattern to remove from the solution: ")))
      (dotnet-command (concat "dotnet sln " solution-file " remove " to-remove)))))

;;;###autoload
(defun dotnet-sln-list ()
  "List all projects in a Solution."
  (interactive)
  (let ((solution-file (read-file-name "Solution file: ")))
    (dotnet-command (concat "dotnet sln " solution-file " list"))))

;;;###autoload
(defun dotnet-sln-new ()
  "Create a new Solution."
  (interactive)
  (let ((solution-path (read-directory-name "Solution path: ")))
    (dotnet-command (concat "dotnet new sln -o " solution-path))))

(defvar dotnet-test-last-test-proj nil
  "Last unit test project file executed by `dotnet-test'.")

;;;###autoload
(defun dotnet-test (arg)
  "Launch project unit-tests, querying for a project on first call.  With ARG, query for project path again."
  (interactive "P")
  (when (or (not dotnet-test-last-test-proj) arg)
    (setq dotnet-test-last-test-proj (read-file-name "Launch tests for Project file: ")))
  (dotnet-command (concat "dotnet test " dotnet-test-last-test-proj)))

(defun golang-command (cmd)
  "Run CMD in an async buffer."
  (detached-compile cmd))

(defun dotnet-find (extension)
  "Search for a EXTENSION file in any enclosing folders relative to current directory."
  (dotnet-search-upwards (rx-to-string extension)
                         (file-name-directory buffer-file-name)))

(defun dotnet-goto (extension)
  "Open file with EXTENSION in any enclosing folders relative to current directory."
  (let ((file (dotnet-find extension)))
    (if file
        (find-file file)
      (error "Could not find any %s file" extension))))

(defun dotnet-goto-sln ()
  "Search for a solution file in any enclosing folders relative to current directory."
  (interactive)
  (dotnet-goto ".sln"))

(defun dotnet-goto-csproj ()
  "Search for a C# project file in any enclosing folders relative to current directory."
  (interactive)
  (dotnet-goto ".csproj"))

(defun dotnet-goto-fsproj ()
  "Search for a F# project file in any enclosing folders relative to current directory."
  (interactive)
  (dotnet-goto ".fsproj"))

(defun dotnet-search-upwards (regex dir)
  "Search for REGEX in DIR."
  (when dir
    (or (car-safe (directory-files dir 'full regex))
        (dotnet-search-upwards regex (dotnet-parent-dir dir)))))

(defun dotnet-parent-dir (dir)
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

(defun dotnet-valid-project-solutions (path)
  "Predicate to validate project/solution paths.  PATH is passed by `'read-file-name`."
  ;; file-attributes returns t for directories
  ;; if not a dir, then check the common extensions
  (let ((extension (file-name-extension path))
        (valid-projects (list "sln" "csproj" "fsproj")))
    (or (member extension valid-projects)
        (car (file-attributes path)))))

(defvar golang-mode-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a p") #'dotnet-add-package)
    (define-key map (kbd "a r") #'dotnet-add-reference)
    (define-key map (kbd "b")   #'dotnet-build)
    (define-key map (kbd "c")   #'golang-mod-tidy)
    (define-key map (kbd "g c") #'dotnet-goto-csproj)
    (define-key map (kbd "g f") #'dotnet-goto-fsproj)
    (define-key map (kbd "g s") #'dotnet-goto-sln)
    (define-key map (kbd "n")   #'golang-new)
    (define-key map (kbd "p")   #'dotnet-publish)
    (define-key map (kbd "r")   #'dotnet-restore)
    (define-key map (kbd "e")   #'golang-run)
    (define-key map (kbd "C-e") #'dotnet-run-with-args)
    (define-key map (kbd "s a") #'dotnet-sln-add)
    (define-key map (kbd "s l") #'dotnet-sln-list)
    (define-key map (kbd "s n") #'dotnet-sln-new)
    (define-key map (kbd "s r") #'dotnet-sln-remove)
    (define-key map (kbd "t")   #'dotnet-test)
    map)
  "Keymap for golang-mode commands after `golang-mode-keymap-prefix'.")

(defvar golang-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd golang-mode-keymap-prefix) golang-mode-command-map)
    map)
  "Keymap for golang-mode.")

;;;###autoload
(define-minor-mode golang-mode
  "dotnet CLI minor mode."
  nil
  " golang"
  golang-mode-map
  :group 'golang
  ;; (add-to-list 'compilation-error-regexp-alist-alist
  ;; (cons 'dotnet dotnet-compilation-regexps))
  ;; (add-to-list 'compilation-error-regexp-alist 'dotnet))
  )

(provide 'k-go)
