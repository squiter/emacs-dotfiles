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
  (path-join *emacs-dotfiles-dir* "emacs.d")
  "Path to emacs.d directory.")

(defconst *wakatime-dir*
  (path-join *user-home-directory* ".local" "bin" "wakatime")
  "Path to the wakatime binary.")

(defconst *all-project-directories*
  (list
   *projects-directory*
   (path-join *user-home-directory* "dev" "nu"))
  "List of all my project directories.")

(provide 'init-constants)
;;; init-constants.el ends here
