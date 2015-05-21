(if (file-exists-p "/usr/local/rbenv")
  (require 'rbenv)

  (setq rbenv-installation-dir "/usr/local")

  (global-rbenv-mode))

(provide 'init-rbenv)
