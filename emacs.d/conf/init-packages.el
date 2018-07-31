;; init-packages.el --- List and management of my packages

;;; commentary:
;; That file contains my-packages list and a dolist function that install each package
;; This idea of managing packages was stolen from: https://github.com/rranelli/emacs-dotfiles

;;; code:

(defvar my-packages
  '(
    ace-window
    achievements
    all-the-icons
    all-the-icons-dired
    apropospriate-theme
    avy
    bundler
    challenger-deep-theme
    discover-my-major
    docker
    doom-themes
    easy-kill
    enh-ruby-mode
    gist
    git-gutter
    git-gutter-fringe
    git-timemachine
    haskell-mode
    highlight
    hl-anything
    hydra
    imenu-list
    ivy-youtube
    iy-go-to-char
    json-navigator
    ledger-mode
    lua-mode
    magit
    magithub
    markdown-mode+
    mutant
    ob-restclient
    ob-sml
    org-bullets
    org-gcal
    ox-twbs
    pdf-tools
    projectile
    projectile-rails
    counsel-projectile
    rainbow-mode
    rbenv
    request
    rhtml-mode
    rspec-mode
    rubocop
    ruby-refactor
    ruby-tools
    symbol-overlay
    smex
    sml-mode
    tagedit
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
