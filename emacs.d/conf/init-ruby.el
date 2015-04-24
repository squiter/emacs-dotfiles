;; RVM
(require 'rvm)
;; use rvm’s default ruby for the current Emacs session
(rvm-use-default)

(require 'rinari)
(require 'rspec-mode)

;; do not add encoding comment automatically
(setq ruby-insert-encoding-magic-comment nil)

(defadvice switch-to-buffer (after fix-current-rvm activate)
  (rvm-activate-corresponding-ruby))

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

;; Rubocop
(add-hook 'ruby-mode-hook 'rubocop-mode)

;;open the spec of a class
(defun senny-ruby-open-spec-other-buffer ()
  (interactive)
  (when (featurep 'rspec-mode)
    (let ((source-buffer (current-buffer))
          (other-buffer (progn
                          (rspec-toggle-spec-and-target)
                          (current-buffer))))
      (switch-to-buffer source-buffer)
      (pop-to-buffer other-buffer))))

(eval-after-load 'ruby-mode
  '(progn
     (define-key ruby-mode-map (kbd "C-c , ,") 'senny-ruby-open-spec-other-buffer)))

;; String interpolation
(defun senny-ruby-interpolate ()
  "In a double quoted string, interpolate."
  (interactive)
  (insert "#")
  (when (and
         (looking-back "\".*")
         (looking-at ".*\""))
    (insert "{}")
    (backward-char 1)))

(eval-after-load 'ruby-mode
  '(progn
     (define-key ruby-mode-map (kbd "#") 'senny-ruby-interpolate)))

(provide 'init-ruby)
