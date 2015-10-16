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
    anzu
    apel
    auto-package-update
    avy
    back-button
    bundler
    company
    company-emoji
    dash-at-point
    discover-my-major
    easy-kill
    erc-hl-nicks
    erc-image
    expand-region
    flycheck
    frame-cmds
    git-gutter
    git-timemachine
    guide-key
    haskell-mode
    helm
    helm-ag
    helm-projectile
    highlight
    highlight-symbol
    ido-vertical-mode
    indent-guide
    magit
    neotree
    projectile
    projectile-rails
    rbenv
    restclient
    rspec-mode
    rubocop
    smartparens
    swiper-helm
    telephone-line
    twittering-mode
    wakatime-mode
    web-mode
    yagist
    yaml-mode
    yasnippet
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
