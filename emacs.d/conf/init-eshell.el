;;; init-eshell.el --- My configuration for Emacs Shell
;;; Commentary:
;;; Code:
(use-package eshell
  :commands (eshell custom/projectile-eshell)
  :bind ("C-c C-t" . 'custom/projectile-eshell)
  :config
  (defun custom/bash-command (&rest cmd)
    "Run CMD as if you were in a bash shell instead of Eshell."
    (insert (format "bash -c 'source ~/.bash_profile; cd %s; %s'"
                    (eshell/pwd)
                    (mapconcat 'identity (car cmd) " ")))
    (eshell-send-input))

  (defun custom/projectile-eshell ()
    "Open an eshell buffer at project's root directory."
    (interactive)
    (let ((shell-dir (projectile-project-root))
          (shell-title (format "*eshell [%s]*" (projectile-project-name))))
      (other-window 1)
      (if (get-buffer shell-title)
          (switch-to-buffer (get-buffer shell-title))
        (switch-to-buffer (generate-new-buffer shell-title))
        (eshell-mode)
        (goto-char (point-max))
        (insert (format "cd %s" shell-dir))
        (eshell-send-input))))

   ;;; Emacs Shell Alias
  (require 'em-alias)
  (add-to-list 'eshell-command-aliases-list (list "ls" "ls -l"))
  (add-to-list 'eshell-command-aliases-list (list "be" "bundle exec $*"))
  (add-to-list 'eshell-command-aliases-list (list "fuck_product" "RAILS_ENV=test rake db:drop db:create db:migrate db:seed")))

(provide 'init-eshell)
;;; init-eshell.el ends here
