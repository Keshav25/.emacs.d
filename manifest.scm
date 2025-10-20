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
			 (gnu packages screen)
			 (gnu packages image)
			 (gnu packages video)
			 (gnu packages time)
			 (gnu packages protobuf)
			 (gnu packages rpc)
			 (guix download)
			 (guix build-system python)
			 (gnu packages python-xyz)
			 (gnu packages python-web)
			 (gnu packages python-crypto)
			 (gnu packages python-build)
			 (gnu packages python-science)
			 (gnu packages python-compression)
			 (gnu packages tree-sitter)
			 (gnu packages libffi)
			 (gnu packages xdisorg)
			 (gnu packages audio)
			 (gnu packages databases)
			 (guix build-system pyproject))

(define-public k-emacs
  (package
   (inherit emacs-next)
   (version "31.0.50")
   (name "k-emacs")
   (inputs (modify-inputs (package-inputs emacs-next)
						  (prepend
						   libxrender
						   libxt)))))



(packages->manifest (list git
						  cmake
						  k-emacs
						  ripgrep
						  picom
						  git-delta
						  git-imerge
						  brightnessctl
						  dtach
						  playerctl
						  ;; nushell
						  flameshot
						  mpv
						  clojure
						  javacc
						  openjdk
						  ;; python-aider-chat
						  graphicsmagick
						  imagemagick
						  distrobox
						  xbindkeys
						  keynav
						  arandr
						  xdotool
						  transset
						  go-github-com-go-ldap-ldap-v3
						  ))
