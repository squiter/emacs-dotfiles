(require 'rspec-mode)

;; auto modes
(dolist (fp '("\\.rb$"
	      "\\.ru$"
              "\\.rake"
	      "\\.jbuilder$"
	      "\\.gemspec$"
	      "\\GuardFile$"
	      "\\Rakefile$"
	      "\\Vagrantfile$"
	      "\\Gemfile$"
	      "\\Godfile$"
	      "\\.god$"))
  (add-to-list 'auto-mode-alist `(,fp . ruby-mode)))

;; do not add encoding comment automatically
(setq ruby-insert-encoding-magic-comment nil)

;; rspec-mode configuration
(add-hook 'ruby-mode-hook 'rspec-mode)

(setq rspec-use-rake-when-possible nil)
(setq rspec-use-spring-when-possible nil)

(provide 'init-ruby)
