;;; http://blogs.fluidinfo.com/terry/2011/11/10/emacs-buffer-mode-histogram/
(defun k-buffer-mode-histogram ()
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

;; should turn into a mode
(defun writing-on ()
  (interactive)
  (writeroom-mode 1)
  (display-line-numbers-mode 0)
  (toggle-truncate-lines 1)
  (toggle-word-wrap 1)
  (org-modern-mode 1))

(defun k-find-file-in-directory (initial-path)
  "find-file in the given path, for example:
    (k-find-file-in-directory \"~/org\")"
  (let ((default-directory
		  ;; Checks to see if the path already has a slash at the end
		  (if (string-suffix-p "/" initial-path)
			  (expand-file-name initial-path)
		  (concat (expand-file-name initial-path) "/"))))
	(call-interactively #'find-file)))

