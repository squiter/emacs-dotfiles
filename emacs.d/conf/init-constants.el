;;; init-constants.el --- Set up constants required during initialization
;;; Commentary:
;;; Code:

(require 'lib/path)
(require 'lib/env)


;;; (Directories) ;;;
(defconst *user-home-directory*
  (getenv-or "HOME" (concat (expand-file-name "~") "/"))
  "Path to user home directory.")

(provide 'init-constants)
;;; init-constants.el ends here
