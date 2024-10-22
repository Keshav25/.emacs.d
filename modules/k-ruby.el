(leaf ruby-mode
  :doc
  :ensure t
  :config
  (setq ruby-insert-encoding-magic-comment nil))

(leaf yard-mode
  :after (ruby)
  :doc "this is for ruby comments"
  :ensure t)

(leaf inf-ruby
  :after (ruby)
  :ensure t
  :hook (compilation-filter-hook . inf-ruby-auto-enter))

(leaf company-inf-ruby
  :after (inf-ruby)
  :ensure t)

(leaf rubocop
  :doc "ruby"
  :after (ruby)
  :ensure t
  :hook (ruby-mode . rubocop-mode))

(leaf robe
  :doc "code lookup for ruby"
  :after (ruby yard-mode)
  :ensure t)

(leaf bundler
  :doc "ruby"
  :after (ruby)
  :ensure t)

(leaf rake
  :doc "makefiles in ruby"
  :after (ruby)
  :ensure t)

(leaf rbenv
  :after (ruby)
  :ensure t)

(leaf rvm
  :after (ruby)
  :ensure t)

(leaf chruby
  :after (ruby)
  :ensure t)

(leaf rspec-mode
  :doc "testing for ruby"
  :after (ruby)
  :ensure t)

(leaf minitest
  :doc "testing for ruby"
  :after (ruby)
  :ensure t)

(leaf ruby-json-to-hash
  :doc "refactoring for ruby"
  :after (ruby)
  :ensure t)

(leaf ruby-on-rails
  :doc "leaf switch for ruby on rails"
  :after (ruby)
  :disabled t)

(leaf rails-routes
  :after (ruby-on-rails)
  :ensure t)

(leaf rails-i18n
  :after (ruby-on-rails)
  :ensure t)

(leaf inflections
  :after (ruby-on-rails)
  :ensure t)
