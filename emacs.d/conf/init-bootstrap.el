;;; init-bootstrap.el --- Bootstrap for my emacs configurations
;;
;; Copyright (C) 2015 Brunno dos Santos <emacs at brunno dot me>
;;
;; Author: Brunno dos Santos @squiter
;; URL: http://github.com/squiter/dotfiles
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; See <http://www.gnu.org/licenses/> for a copy of the GNU General
;; Public License.
;;
;;; Commentary:
;;
;; Thanks to http://github.com/rranelli/emacs-dotfile

;;; Code:

(add-to-list 'load-path (expand-file-name "vendor" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "vendor/org/lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "vendor/org/contrib/lisp" user-emacs-directory) t)

(defvar init-files
  '(init-secrets
    init-constants
    init-packages
    init-general
    init-custom-functions
    init-edit-custom-functions
    init-ui
    init-ivy
    init-lsp
    init-mac-switch-meta
    init-smerge
    init-dired
    init-sml
    init-magit
    init-projectile
    init-path
    init-ruby
    init-web-mode
    init-company
    init-gitgutter
    init-shell
    init-eshell
    init-ido
    init-bindings
    init-smartparens
    init-org
    init-wakatime
    init-flycheck
    init-flyspell
    init-clojure
    init-restclient
    init-yasnippet
    init-tramp
    init-twittering-mode
    init-haskell
    init-which-key
    init-markdown
    vkill
    init-artist-mode
    init-git-pr
    init-langtool
    init-google
    vmd-mode
    init-scratch
    init-prettify-symbols
    init-ctags
    init-hydra
    init-elfeed
    rcodetools
    init-elixir
    init-javascript
    init-rust
    init-keybindings))

(defun safe-require (feature)
  "Safely requires FEATURE."
  (condition-case ex
      (require feature)
    ('error (add-to-list 'rr/initialization-errors
                         (format "[ERROR LOADING \"%s\"]: %s" (symbol-name feature) ex)))))

(defun rr/safe-load-init-files ()
  "This function wrap the require method to capture error messages."
  (dolist (file init-files)
    (safe-require file)))

(defun rr/unsafe-load-init-files ()
  "This function only load the files with a require."
  (dolist (file init-files)
    (require file)))

(provide 'init-bootstrap)
;;; init-bootstrap.el ends here
