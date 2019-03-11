;;; init-path.el --- My configurations to $PATH
;;; Commentary:
;;  The $PATH env var is a problem in OSX Emacs's version
;;  This code solve that kind of problems
;;; Code:

;; if OSX

(use-package exec-path-from-shell
       :ensure t
       :config
       (exec-path-from-shell-initialize))

(provide 'init-path)
;;; init-path.el ends here
