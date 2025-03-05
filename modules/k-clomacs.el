(leaf clomacs
  :ensure t
  :require t
  :config
  (clomacs-httpd-start))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (clomacs-defun get-property System/getProperty) ;;
;; (message (get-property "java.version"))		   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(leaf cm-test
  :config 
  (add-to-list 'load-path "~/.emacs.d/clomacs/cm-test/src/elisp/")
  (require 'cm-test))
