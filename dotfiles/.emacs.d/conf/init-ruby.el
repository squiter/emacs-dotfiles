;;; init-ruby.el --- My Ruby configurations
;;; Commentary:
;;; Code:

(use-package enh-ruby-mode
  :init
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
    (add-to-list 'auto-mode-alist `(,fp . enh-ruby-mode)))

  :config
  (autoload 'enh-ruby-mode "enh-ruby-mode" "Major mode for ruby files" t)
  (add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))

  ;; Enhanced Ruby Mode defines its own specific faces with the hook
  ;; erm-define-faces. If your theme is already defining those faces, to
  ;; not overwrite them, just remove the hook with:
  (remove-hook 'enh-ruby-mode-hook 'erm-define-faces)

  ;; do not add encoding comment automatically
  (setq ruby-insert-encoding-magic-comment nil)

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

  ;; vcr toggle
  (defun custom/vcr-toggle ()
    (interactive)
    (if (getenv "VCR_OFF")
        (progn
          (setenv "VCR_OFF" nil)
          (message "VCR is ON"))
      (setenv "VCR_OFF" "true")
      (message "VCR is OFF")))

  ;; debbuger utilities
  (defun rr/pry-byebug-jump-to-source ()
    "Jumps to source location given debugger output"
    (interactive)
    (delete-other-windows)
    (when (save-excursion (search-backward-regexp "From: \\(.*\.rb\\) @ line \\([0-9]+\\)")))
    (let ((file (match-string 1))
          (line (string-to-int (match-string 2))))
      (find-file-other-window file)
      (goto-line line)))

  ;; -- GODAMMIT RUBY INDENTATION!!! --
  ;; don't indent parenthesis in a weird way
  (setq ruby-align-chained-calls nil
        ruby-align-to-stmt-keywords nil
        ruby-deep-indent-paren nil
        ruby-deep-indent-paren-style nil
        ruby-use-smie nil)

  (defadvice ruby-indent-line (after unindent-closing-paren activate)
    "Indent sole parenthesis in loca's way."
    (let ((column (current-column))
          indent offset)
      (save-excursion
        (back-to-indentation)
        (let ((state (syntax-ppss)))
          (setq offset (- column (current-column)))
          (when (and (eq (char-after) ?\))
                     (not (zerop (car state))))
            (goto-char (cadr state))
            (setq indent (current-indentation)))))
      (when indent
        (indent-line-to indent)
        (when (> offset 0) (forward-char offset)))))

  ;; This functions requires projectile package
  (defun squiter/rails-go-to-last-migration ()
    (interactive)
    (find-file
     (concat (projectile-expand-root "db/migrate/")
             (car (reverse
                   (directory-files
                    (expand-file-name
                     (projectile-expand-root "db/migrate/"))
                    nil
                    "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)"))))))

  (defcustom endless/ruby-extensions-file
    "../console_extensions.rb"
    "File loaded when a ruby console is started.
Name is relative to the project root.")

  ;; Skip ENV prompt that shows up in some cases.
  (setq inf-ruby-console-environment "development")

  (defun endless/run-ruby ()
    (interactive)
    (require 'inf-ruby)
    (let ((default-directory (projectile-project-root))
          (was-running (get-buffer-process inf-ruby-buffer)))
      ;; This function automatically decides between starting
      ;; a new console or visiting an existing one.
      (inf-ruby-console-auto)
      (when (and (not was-running)
                 (get-buffer-process (current-buffer))
                 (file-readable-p endless/ruby-extensions-file))
        ;; If this brand new buffer has lots of lines then
        ;; some exception probably happened.
        (send-string
         (get-buffer-process (current-buffer))
         (concat "require '" endless/ruby-extensions-file
                 "'\n")))))

  )

(use-package rspec-mode
  :config
  ;; rspec-mode configuration
  (setq rspec-use-rake-when-possible nil)
  (setq rspec-use-spring-when-possible nil)
  (setq compilation-scroll-output t)

  (add-hook 'dired-mode-hook 'rspec-dired-mode)

  ;;open the spec of a class
  (defun senny-ruby-open-spec-other-buffer ()
    (interactive)
    (when (featurep 'rspec-mode)
      (let ((source-buffer (current-buffer))
            (other-buffer (progn
                            (rspec-toggle-spec-and-target)
                            (current-buffer))))
        (switch-to-buffer source-buffer)
        (pop-to-buffer other-buffer)))))

(use-package ruby-refactor
  :config
  (add-hook 'ruby-mode-hook 'ruby-refactor-mode-launch)
  (add-hook 'enh-ruby-mode-hook 'ruby-refactor-mode-launch))

(use-package mutant
  :config
  (add-hook 'ruby-mode-hook 'mutant-mode)
  (add-hook 'enh-ruby-mode-hook 'mutant-mode))

(use-package rubocop
  :config
  (add-hook 'ruby-mode-hook 'rubocop-mode)
  (add-hook 'enh-ruby-mode-hook 'rubocop-mode))

(use-package ruby-tools)
(use-package bundler)

(use-package rbenv
  :config
  (setq rbenv-installation-dir "/usr/local")
  (setq rbenv-show-active-ruby-in-modeline nil)

  (global-rbenv-mode))

(provide 'init-ruby)
;;; init-ruby.el ends here
