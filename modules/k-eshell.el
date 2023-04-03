;; Eshell mycat command
;; To know if I need to print as image or I need to default to the default eshell/cat
(defun my-is-imagep (filename)
  (let ((extension (file-name-extension filename))
        (image-extensions '("png" "jpg" "bmp")))
    (member extension image-extensions)))

;; Creates a space with display properties. Feel free to change `eshell/println` to `insert` and use it in a normal emacs buffer, it will inline the path given in `file`.
(defun my-print-image-eshell (file)
  (eshell/printnl (propertize " " 'display (create-image file))))


;; If image, use `my-print-image-eshell`. Otherwise, just use `eshell/cat`.
(defun eshell/mycat (&rest args)
  (interactive)
  (mapc (lambda (arg)
		  (if (my-is-imagep arg)
			  (my-print-image-eshell arg)
			(eshell/cat arg)))
		(-flatten args))
  nil)

(provide 'k-eshell)
