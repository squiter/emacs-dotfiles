;;; init-rbenv.el --- My configration for Rbenv
;;; Commentary:
;;; Code:

(require 'rbenv)

(setq rbenv-installation-dir "/usr/local")

(global-rbenv-mode)

(setq rbenv-modeline-function 'rbenv--modeline-plain)
(setq rbenv-show-active-ruby-in-modeline nil)

(provide 'init-rbenv)
;;; init-rbenv.el ends here
