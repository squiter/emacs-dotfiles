(setenv "SHELL" "/bin/zsh")
(setenv "ESHELL" "/bin/zsh")

(defun set-exec-path-from-shell-PATH ()
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(when (window-system)
  (set-exec-path-from-shell-PATH))

(provide 'init-path)
