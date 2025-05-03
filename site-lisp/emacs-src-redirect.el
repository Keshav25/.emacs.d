;;; emacs-src-redirect.el --- Redirect lookups from installed Emacs to local source -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Paul D. Nelson

;; Author: Paul D. Nelson <ultrono@gmail.com>
;; Keywords: convenience, lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package redirects lookups from installed Emacs Lisp files
;; (.el.gz) to their corresponding source files in your local Emacs
;; source tree.  This is useful when you're working on Emacs itself
;; and want to use `xref' to navigate to definitions in your working
;; copy rather than the installed files.
;;
;; Usage: customize the variable `emacs-src-redirect-lisp-dir', then:
;;
;;   M-x emacs-src-redirect-mode

;;; Code:

(defgroup emacs-src-redirect nil
  "Redirects `xref' lookups from installed Emacs files to local source."
  :group 'programming
  :prefix "emacs-src-redirect-")

(defcustom emacs-src-redirect-lisp-dir "~/gnu-emacs/lisp"
  "Directory containing Emacs Lisp source files (.el)."
  :type 'directory)

(defun emacs-src-redirect-file (file)
  "Redirect installed Emacs Lisp FILE to its source file counterpart."
  (if (and file
           (string-prefix-p (file-name-as-directory (expand-file-name lisp-directory))
                            (expand-file-name file)))
      (let* ((relative-path (file-relative-name file lisp-directory))
             (basename (file-name-nondirectory relative-path))
             (dirname (file-name-directory relative-path))
             (base-no-gz (if (string-match "\\.gz$" basename)
                             (substring basename 0 (match-beginning 0))
                           basename))
             (source-path (if dirname
                              (expand-file-name (file-name-concat dirname base-no-gz) 
                                                emacs-src-redirect-lisp-dir)
                            (expand-file-name base-no-gz emacs-src-redirect-lisp-dir))))
        (if (file-exists-p source-path)
            source-path
          file))
    file))

(defun emacs-src-redirect-find-lisp-object (orig-fun &rest args)
  "Redirect `find-lisp-object-file-name' to use source files.
ORIG-FUN is the original function, ARGS are the arguments passed to it."
  (let ((result (apply orig-fun args)))
    (if (and result (stringp result))
        (emacs-src-redirect-file result)
      result)))

(defun emacs-src-redirect-function-search (orig-fun symbol type file)
  "Redirect the symbol search to the source files.
ORIG-FUN is the original function, which searches for SYMBOL's
definition of type TYPE in FILE."
  (let ((redirected-file (emacs-src-redirect-file file)))
    (apply orig-fun (list symbol type redirected-file))))

(defun emacs-src-redirect-xref-location (orig-fun symbol type file)
  "Redirect xref location creation to use source files.
ORIG-FUN is the original function, which searches for SYMBOL's
definition of type TYPE in FILE."
  (funcall orig-fun symbol type (emacs-src-redirect-file file)))

(defun emacs-src-redirect-find-library-name (orig-fun &rest args)
  "Redirect find-library-name to use source files.
ORIG-FUN is the original function, ARGS are the arguments passed to it."
  (let ((result (apply orig-fun args)))
    (if (and result (stringp result))
        (emacs-src-redirect-file result)
      result)))

(defun emacs-src-redirect-find-library--from-load-history (orig-fun &rest args)
  "Redirect find-library--from-load-history to use source files.
ORIG-FUN is the original function, ARGS are the arguments passed to it."
  (let ((result (apply orig-fun args)))
    (if (and result (stringp result))
        (emacs-src-redirect-file result)
      result)))

(defun emacs-src-redirect-locate-library (orig-fun &rest args)
  "Redirect `locate-library' to use source files.
ORIG-FUN is the original function, ARGS are the arguments passed to it."
  (let ((result (apply orig-fun args)))
    (if (and result (stringp result))
        (emacs-src-redirect-file result)
      result)))

(defun emacs-src-redirect-setup ()
  "Setup advices for redirecting to Emacs source files."
  (advice-add 'find-lisp-object-file-name :around
              #'emacs-src-redirect-find-lisp-object)
  (advice-add 'find-function-search-for-symbol :around
              #'emacs-src-redirect-function-search)
  (advice-add 'xref-make-elisp-location :around
              #'emacs-src-redirect-xref-location)
  (advice-add 'find-library-name :around
              #'emacs-src-redirect-find-library-name)
  (advice-add 'find-library--from-load-history :around
              #'emacs-src-redirect-find-library--from-load-history)
  (advice-add 'locate-library :around
              #'emacs-src-redirect-locate-library))

(defun emacs-src-redirect-teardown ()
  "Remove advices for redirecting to Emacs source files."
  (advice-remove 'find-lisp-object-file-name
                 #'emacs-src-redirect-find-lisp-object)
  (advice-remove 'find-function-search-for-symbol
                 #'emacs-src-redirect-function-search)
  (advice-remove 'xref-make-elisp-location
                 #'emacs-src-redirect-xref-location)
  (advice-remove 'find-library-name
                 #'emacs-src-redirect-find-library-name)
  (advice-remove 'find-library--from-load-history
                 #'emacs-src-redirect-find-library--from-load-history)
  (advice-remove 'locate-library
                 #'emacs-src-redirect-locate-library))

;;;###autoload
(define-minor-mode emacs-src-redirect-mode
  "Toggle redirection from installed Emacs to local source.
When enabled, redirects from installed .el.gz files to your local source
files in `emacs-src-redirect-lisp-dir'."
  :global t
  :lighter nil
  (if emacs-src-redirect-mode
      (emacs-src-redirect-setup)
    (emacs-src-redirect-teardown)))

(provide 'emacs-src-redirect)
;;; emacs-src-redirect.el ends here
