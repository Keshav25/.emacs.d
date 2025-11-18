(make-variable-buffer-local
 (defvar tcr-mode))

(defvar-local git-commit-cmd "git commit -am 'TCR: test OK'")
(defvar-local git-reset-cmd "jj undo")

(defun tcr--execute-git-cmd (git-cmd)
  "Call the given GIT-CMD with 'shell-command."
  (message "calling: %s" git-cmd)
  (shell-command git-cmd))

(defun tcr-run-vcr-revert ()
  "Run a revert operation."
  (when (member 'tcr-mode minor-mode-list)
    (save-buffer)
    (message "TCR: running revert!")
    (tcr--execute-git-cmd git-reset-cmd)))

(defun tcr-run-vcr-commit ()
  "Run a commit operation."
  (when (member 'tcr-mode minor-mode-list)
    (save-buffer)
    (message "TCR: running commit!")
    (tcr--execute-git-cmd git-commit-cmd)))

(define-minor-mode tcr-mode
  "TCR - Test && Commit || Revert."
  :lighter " TCR")


(provide 'tcr-mode)
;;; tcr-mode.el ends here
