(setq
 ;; default directory
 default-directory (concat (getenv "HOME") "/projetos/")
 ;; disable backup files
 make-backup-files nil
 auto-save-default nil
 backup-inhibited t
 ;; make indentation commands use space only
 indent-tabs-mode nil
)

;; import PATH environment variable
(let ((path (shell-command-to-string ". ~/.zshrc; echo -n $PATH")))
  (setenv "PATH" path)
  (setq exec-path
        (append (split-string-and-unquote path ":") exec-path)))

;; whitespace display
(global-whitespace-mode)
(setq whitespace-global-modes
      '(not magit-mode git-commit-mode))
(setq whitespace-style '(face trailing tabs))

(provide 'init-general)
