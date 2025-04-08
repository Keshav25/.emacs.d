(defgroup k-clojure nil
  "dotnet group."
  :prefix "k-clojure-"
  :group 'tools)

(defun k-clojure-select-project ()
  "Prompt for the project/solution file or directory.  Try project root first, else use current buffer's directory."
  (let ((default-dir-prompt "?"))
    (ignore-errors
      (when (fboundp 'project-root)
        (setq default-dir-prompt (project-root))))
    (when (string= default-dir-prompt "?")
      (setq default-dir-prompt default-directory))
    (expand-file-name (read-file-name "Project or solution: " default-dir-prompt nil t))))

(defun golang-add-package (package-name)
  "Add package reference from PACKAGE-NAME."
  (interactive "sPackage name: ")
  (dotnet-command (concat "go get " package-name)))

(defun lein-new (project-path name)
  "Initialize a new Clojure project."
  (interactive (list (read-directory-name "Project path: ")
                     (read-string "Name: ")))
  (let ((default-directory project-path))
    (shell-command (concat "lein new app " name))))

(defun lein-run ()
  "Build a Clojure project."
  (interactive)
  (let* ((target (k-clojure-select-project))
         (command "lein run \"%s\""))
    (compile (format command target))))

(defun lein-uberjar ()
  "create a jar with lein"
  (interactive)
  (let* ((command "lein uberjar"))
    (compile command)))

(defun clojure-jar-run ()
  "run a java jar in a clojure project"
  (interactive)
  (let* ((target (k-clojure-select-project))
         (command "java -jar %s/target/uberjar/*-SNAPSHOT-standalone.jar"))
    (compile (format command target))))
