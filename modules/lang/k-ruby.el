(leaf ruby-mode
  :elpaca t
  :config
  (setq ruby-insert-encoding-magic-comment nil))

(leaf yard-mode
  :after (ruby)
  :doc "this is for ruby comments"
  :elpaca t)

(leaf inf-ruby
  :after (ruby)
  :elpaca t
  :hook (compilation-filter-hook . inf-ruby-auto-enter))

(leaf company-inf-ruby
  :after (inf-ruby)
  :elpaca t)

(leaf rubocop
  :doc "ruby"
  :after (ruby)
  :elpaca t
  :hook (ruby-mode . rubocop-mode))

(leaf robe
  :doc "code lookup for ruby"
  :after (ruby yard-mode)
  :elpaca t)

(leaf bundler
  :doc "ruby"
  :after (ruby)
  :elpaca t)

(leaf rake
  :doc "makefiles in ruby"
  :after (ruby)
  :elpaca t)

(leaf rbenv
  :after (ruby)
  :elpaca t)

(leaf rvm
  :after (ruby)
  :elpaca t)

(leaf chruby
  :after (ruby)
  :elpaca t)

(leaf rspec-mode
  :doc "testing for ruby"
  :after (ruby)
  :elpaca t)

(leaf minitest
  :doc "testing for ruby"
  :after (ruby)
  :elpaca t)

(leaf ruby-json-to-hash
  :doc "refactoring for ruby"
  :after (ruby)
  :elpaca t)

(leaf ruby-on-rails
  :doc "leaf switch for ruby on rails"
  :after (ruby)
  :disabled t)

(leaf rails-routes
  :after (ruby-on-rails)
  :elpaca t)

(leaf rails-i18n
  :after (ruby-on-rails)
  :elpaca t)

(leaf inflections
  :after (ruby-on-rails)
  :elpaca t)
