(defgroup tcr-mode nil
  "Customization group for ShannonMax."
  :group 'local
  :prefix "tcr")

(make-variable-buffer-local
 (defvar tcr-mode))

(defvar-local git-commit-cmd "git commit -am 'TCR: test OK'")
(defvar-local git-reset-cmd "jj undo")

(defun tcr--execute-git-cmd (git-cmd)
  (interactive)
  "Call the given GIT-CMD with 'shell-command."
  (message "calling: %s" git-cmd)
  (shell-command git-cmd))

(defun tcr-run-vcr-revert ()
  (interactive)
  "Run a revert operation."
  (when (member 'tcr-mode minor-mode-list)
    (save-buffer)
    (message "TCR: running revert!")
    (tcr--execute-git-cmd git-reset-cmd)))

(defun tcr-run-vcr-commit ()
  (interactive)
  "Run a commit operation."
  (when (member 'tcr-mode minor-mode-list)
    (save-buffer)
    (message "TCR: running commit!")
    (tcr--execute-git-cmd git-commit-cmd)))

(define-minor-mode tcr-mode
  "TCR - Test && Commit || Revert."
  :lighter " TCR"
  :keymap (let ((map (make-sparse-keymap)))
			(define-key map (kbd "C-x C-s") #'tcr-run-vcr-commit)
			map))

(provide 'tcr-mode)
;;; tcr-mode.el ends here
