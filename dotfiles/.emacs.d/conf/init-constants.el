;;; init-constants.el --- Set up constants required during initialization
;;; Commentary:
;;; Code:

(require 'lib/path)
(require 'lib/env)

;;; (Directories) ;;;
(defconst *user-home-directory*
  (getenv-or "HOME" (concat (expand-file-name "~") "/"))
  "Path to user home directory.")

(defconst *user-dropbox-directory*
  (path-join *user-home-directory* "Dropbox")
  "Path to Dropbox on user's machine.")

(defconst *elixir-ls-path*
  (path-join *user-home-directory* "bin" "elixir-ls")
  "Path to user's binaries of elixir-ls.")

(defconst *user-ebook-directory*
  (path-join *user-dropbox-directory* "e-Books"))

(defconst *user-elfeed-directory*
  (path-join *user-dropbox-directory* "elfeed"))

(defconst *projects-directory*
  (path-join *user-home-directory* "dev" "code")
  "Path to my default project directory.")

;; (defconst *dotfiles-directory*
;;   (path-join *user-home-directory* "dotfiles")
;;   "Path to ~/dotfiles directory.")

(defconst *emacs-dotfiles-dir*
  (path-join *projects-directory* "emacs-dotfiles")
  "Path to emacs-dotfiles.")

(defconst *emacsd-directory*
  (path-join *emacs-dotfiles-dir* "dotfiles/.emacs.d")
  "Path to emacs.d directory.")

(defconst *nixos-current-binaries*
  (path-join "/" "run" "current-system" "sw" "bin")
  "Path to the current binaries of NixOS ")

(defconst *wakatime-nix-dir*
  (path-join *nixos-current-binaries* "wakatime")
  "Path to the wakatime binary in NixOS.")

(defconst *wakatime-dir*
  (path-join *user-home-directory* ".local" "bin" "wakatime")
  "Path to the wakatime binary.")

(defconst *wakatime-osx-dir*
  (path-join "/" "opt" "homebrew" "bin" "wakatime-cli")
  "Path to the wakatime binary.")

(defconst *all-project-directories*
  (list
   *projects-directory*
   (path-join *user-home-directory* "dev" "remote")
   (path-join *user-home-directory* "dev" "nu")
   (path-join *user-home-directory* "dev" "finbits"))
  "List of all my project directories.")

;; Set those temporarily to wait until init-secrets.el
(setq *youtube-key* "temp")
(setq *google-calendar-client-id* "temp")
(setq *google-calendar-secret-id* "temp")

(provide 'init-constants)
;;; init-constants.el ends here
