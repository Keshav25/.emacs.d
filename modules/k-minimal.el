;; This is for whenever I don't have an emacs server running and I don't want to load my entire configuration:

;; theme - doesn't matter which one, just has to be dark mode and a default theme
(load-theme 'modus-vivendi t)

;; keyboard translate for ISRT
(key-translate "C-t" "C-x")
(key-translate "C-x" "C-t")

;; viper - incase for some reason I want vi emulation
(setq viper-expert-level 5
	  viper-ex-style-editing nil
	  viper-ex-style-motion nil)
