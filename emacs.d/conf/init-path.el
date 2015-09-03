;;; init-path.el --- My configurations to $PATH
;;; Commentary:
;;  The $PATH env var is a problem in OSX Emacs's version
;;  This code solve that kind of problems
;;; Code:

(defun set-exec-path-from-shell-PATH ()
  "Function that set $PATH env var."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(when (window-system)
  (set-exec-path-from-shell-PATH))

(provide 'init-path)
;;; init-path.el ends here
