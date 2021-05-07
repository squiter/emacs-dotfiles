;;; init-ui.el --- Configurations for Emacs UI
;;; Commentary:
;;; Code:

;; theme options
(add-to-list 'custom-theme-load-path (concat *emacsd-directory* "/themes"))
(add-to-list 'load-path (concat *emacsd-directory* "/themes"))

(load-theme 'shades-of-purple t)
;; (load-theme 'challenger-deep t)
;; (load-theme 'apropospriate-light t)

(global-display-line-numbers-mode)

;; Highlight current line
(global-hl-line-mode 1)

;; Undo and Redo windows
(winner-mode 1)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; Transparency
(set-frame-parameter (selected-frame) 'alpha '(99 99))
(add-to-list 'default-frame-alist '(alpha 98 98))

(defun squiter/get-window-name ()
  "Get a project name or buffer name."
  (if (projectile-project-p)
      (projectile-project-name)
    (buffer-name)))

(setq frame-title-format
      '("emacs@"
        (:eval (squiter/get-window-name))))

;; (require 'init-telephone)
(require 'init-powerline)

(provide 'init-ui)
;;; init-ui.el ends here
