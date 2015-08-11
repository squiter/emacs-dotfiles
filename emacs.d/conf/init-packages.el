;;; init=packages.el --- List and management of my packages

;;; commentary:
;; That file contains my-packages list and a dolist function that install each package
;; This idea of managing packages was stolen from: https://github.com/rranelli/emacs-dotfiles

;;; code:
(require 'package)
(package-initialize)

(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)

(defvar my-packages
  '(
    ace-window
    auto-package-update
    avy
    bundler
    company
    dash-at-point
    discover-my-major
    easy-kill
    expand-region
    flycheck
    git-gutter
    git-timemachine
    guide-key
    helm
    helm-ag
    helm-projectile
    ido-vertical-mode
    magit
    neotree
    projectile
    projectile-rails
    rbenv
    restclient
    rspec-mode
    rubocop
    smartparens
    wakatime-mode
    web-mode
    yaml-mode
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

(provide 'init-packages)
;;; init-packages ends here
