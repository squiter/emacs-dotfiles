;; init-packages.el --- List and management of my packages

;;; commentary:
;; That file contains my-packages list and a dolist function that install each package
;; This idea of managing packages was stolen from: https://github.com/rranelli/emacs-dotfiles

;;; code:

(defvar my-packages
  '(
    bundler
    enh-ruby-mode
    mutant
    rbenv
    rhtml-mode
    rspec-mode
    rubocop
    ruby-refactor
    ruby-tools
  )
  "A list of packages to be installed at application lauch.")

;; package loading (stolen from chuck that stoled from milhouse)
(setq packaged-contents-refreshed-p nil)
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (condition-case ex
  (package-install p)
  ('error (if packaged-contents-refreshed-p
      (error ex)
    (package-refresh-contents)
    (setq packaged-contents-refreshed-p t)
    (package-install p))))))

(require 'init-simple-packages)

(provide 'init-packages)
;;; init-packages ends here
