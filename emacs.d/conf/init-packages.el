;; init=packages.el --- List and management of my packages

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
    achievements
    anzu
    apel
    auto-package-update
    avy
    back-button
    bundler
    cider
    clojure-mode
    clojure-mode-extra-font-locking
    company
    company-emoji
    dired+
    discover-my-major
    docker
    easy-kill
    expand-region
    flycheck
    frame-cmds
    free-keys
    gist
    git-gutter
    git-gutter-fringe
    git-timemachine
    google-this
    google-translate
    haskell-mode
    highlight
    hl-anything
    hydra
    ido-vertical-mode
    indent-guide
    imenu-list
    ivy-youtube
    langtool
    magit
    markdown-mode+
    multiple-cursors
    mutant
    neotree
    ob-restclient
    ob-sml
    org-bullets
    org-gcal
    ox-twbs
    projectile
    projectile-rails
    counsel-projectile
    rainbow-delimiters
    rainbow-mode
    rbenv
    request
    restclient
    rhtml-mode
    rspec-mode
    rubocop
    ruby-tools
    smartparens
    smart-shift
    smex
    sml-mode
    swiper
    tagedit
    telephone-line
    try
    twittering-mode
    undo-tree
    wakatime-mode
    web-mode
    which-key
    yagist
    yaml-mode
    yasnippet
    zeal-at-point
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
