(use-modules (guix profiles)
			 (guix packages)
			 (guix transformations)
			 (gnu packages emacs)
			 (gnu packages xorg)
			 (gnu packages version-control)
			 (gnu packages cmake)
			 (gnu packages rust-apps)
			 (gnu packages compton))

(define-public k-emacs
  (package
   (inherit emacs-next)
   (name "k-emacs")
   (inputs (modify-inputs (package-inputs emacs-next)
						  (prepend
						   libxrender
						   libxt)))))

(packages->manifest (list
					 git
					 cmake
					 k-emacs
					 ripgrep
					 picom
					 git-delta))
