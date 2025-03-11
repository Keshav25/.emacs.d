(use-modules (guix profiles)
			 (guix packages)
			 (guix transformations)
			 (gnu packages emacs)
			 (gnu packages xorg)
			 (gnu packages version-control)
			 (gnu packages cmake)
			 (gnu packages rust-apps)
			 (gnu packages compton)
			 (gnu packages linux)
			 (gnu packages screen))

(define-public k-emacs
  (package
   (inherit emacs-next)
   (version "31.0.50")
   (name "k-emacs")
   (inputs (modify-inputs (package-inputs emacs-next)
						  (prepend
						   libxrender
						   libxt)))))

(packages->manifest '(git
					 cmake
					 k-emacs
					 ripgrep
					 picom
					 git-delta
					 brightnessctl
					 dtach
					 nushell
					 flameshot
					 mpv))
