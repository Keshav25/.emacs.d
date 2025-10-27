;; -*- lexical-binding: t -*-

(leaf dashboard
  :disabled t
  :elpaca t
  :after org
  :config
  (dashboard-setup-startup-hook)
  :setq
  (initial-buffer-choice . (lambda () (get-buffer-create "*dashboard*")))
  (dashboard-center-content . t)
  (dashboard-show-shortcuts . t)
  (dashboard-items .'((recents . 5)
					  (bookmarks . 5)
					  (agenda . 5)
					  (registers . 5)))
  (dashboard-set-heading-icons . t)
  (dashboard-set-file-icons . t)
  (dashboard-set-navigator . t)
  (dashboard-week-agenda . t)
  (dashboard-filter-agenda-entry . 'dashboard-no-filter-agenda)
  :config
  (setq dashboard-banner-logo-title (concat "Welcome " user-full-name))
  (leaf org-dashboard :elpaca t)
  (leaf dashboard-ls :elpaca t)
  (leaf elfeed-dashboard :elpaca t)
  (leaf dashboard-hackernews :elpaca t))

(leaf grid
  :disabled t
  :elpaca (grid :host github :repo "https://github.com/ichernyshovvv/grid.el")
  :require t)

(leaf enlight
  :disabled t
  :after grid
  :elpaca t
  :require t 
  :custom
  (initial-buffer-choice . #'enlight)
  :config
  (defvar enlight-lipsum "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.

Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.")
  (defface enlight-yellow-bold
	'((t (:foreground "#cabf00" :bold t)))
	"Yellow bold face")

  (defvar enlight-guix
	(propertize
	 " ..                             `.
 `--..```..`           `..```..--`   
   .-:///-:::.       `-:::///:-.     
      ````.:::`     `:::.````        
           -//:`    -::-             
            ://:   -::-              
            `///- .:::`              
             -+++-:::.               
              :+/:::-                
              `-....`                "
	 'face 'enlight-yellow-bold))
  (defvar enlight-guix-widget
	`( :content ,(concat "\n" (propertize "Block 1" 'face 'enlight-yellow-bold)
						 "\nGUIX MANAGEMENT WIDGET\n\n")
       :width 22 :border t :align center :padding 2))

  (defvar enlight-email-width
	`( :content
       ,(concat "\n" (propertize "Block 2" 'face 'enlight-yellow-bold)
				"\nEMAIL WIDGET\n\n")
       :padding 2 :width 22 :align center :border t))

  (defvar enlight-weather-width
	`( :content
       ,(concat "\n" (propertize "Block 3" 'face 'enlight-yellow-bold)
				"\nWEATHER WIDGET\n\n")
       :padding 2 :width 22 :border t :align center))

  (defvar enlight-calendar
	(progn
      (calendar)
      (diary-mark-entries)
      (prog1 (with-current-buffer (buffer-name (current-buffer))
			   (buffer-string))
		(calendar-exit))))
  (setopt enlight-content (concat
						   (grid-make-box `(:align center :content ,enlight-guix :width 80))
						   (grid-make-row (list (grid-make-box
												 (concat
												  (grid-make-box
												   `(:content
													 ,(concat
													   (grid-make-box `(:content ,(propertize "HEADER" 'face 'highlight)
																				 :with 80 :align center))
													   (grid-make-row
														`(,enlight-guix-widget
														  "     "
														  ,enlight-email-width
														  "      "
														  ,enlight-weather-width)))
													 :width 80))
												  "\n\n\n"
												  enlight-calendar "\n"
												  (grid-make-row
												   `( ,(concat

														(propertize "MENU" 'face 'highlight)
														"\n"

														(enlight-menu
														 '(("Org Mode"
															("Org Agenda (current day)" (org-ql-view "agenda") "a"))
														   ("Downloads"
															("Downloads folder" (dired "~/Downloads") "d"))
														   ("Other"
															("Projects" project-switch-project "p"))))

														(grid-make-column
														 `(,(propertize "THINGS TO REMEMBER" 'face 'highlight)
														   (:content ,enlight-lipsum :width 50)))))))))))))

(provide 'k-dashboard)
