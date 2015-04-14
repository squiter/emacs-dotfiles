;; RVM
(require 'rvm)
;; use rvm’s default ruby for the current Emacs session
(rvm-use-default)

;; Ruby
(require 'rspec-mode)

;; do not add encoding comment automatically
(setq ruby-insert-encoding-magic-comment nil)

;; RVM + Rspec

;; To run rspec-mode with bash (fix rvm problems)
(defadvice rspec-compile (around rspec-compile-around)
  "Use BASH shell for running the specs because of ZSH issues."
  (let ((shell-file-name "/bin/bash"))
    ad-do-it))

(setq-default rspec-use-rvm t)

;; this hook enable debug in rspec-mode
(add-hook 'after-init-hook 'inf-ruby-switch-setup)

;; rspec-mode configuration

(setq rspec-use-rake-when-possible nil)
(setq rspec-use-spring-when-possible nil)
(setq compilation-scroll-output t)

(ad-activate 'rspec-compile)

(add-hook 'ruby-mode-hook
	  (lambda ()
	    (rvm-activate-corresponding-ruby)))

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

(require 'rubocop)

(provide 'init-ruby)
