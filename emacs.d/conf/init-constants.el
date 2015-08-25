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
  (path-join *user-home-directory* "dropbox")
  "Path to Dropbox on user's machine.")

(defconst *dotfiles-directory*
  (path-join *user-home-directory* "dotfiles")
  "Path to ~/dotfiles directory.")

(defconst *emacsd-directory*
  (path-join *dotfiles-directory* "emacs.d")
  "Path to emacs.d directory.")

(provide 'init-constants)
;;; init-constants.el ends here
