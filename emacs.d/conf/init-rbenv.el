;;; init-rbenv.el --- My configration for Rbenv
;;; Commentary:
;;; Code:

(require 'rbenv)

(setq rbenv-installation-dir "/usr/local")

(setq rbenv-show-active-ruby-in-modeline nil)

(global-rbenv-mode)

(provide 'init-rbenv)
;;; init-rbenv.el ends here
