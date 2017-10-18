;;; init-ui.el --- Configurations for Emacs UI
;;; Commentary:
;;; Code:

;; theme options
(add-to-list 'custom-theme-load-path (concat *emacsd-directory* "/themes"))
(add-to-list 'load-path (concat *emacsd-directory* "/themes"))

(load-theme 'challenger-deep t)

(setq
 ;; better startup
 inhibit-splash-screen t
 inhibit-startup-message t
 ;; show column number at bottom bar
 column-number-mode t
 ;; disable anoying beep
 ring-bell-function 'ignore
 ;; improve rendering performance
 redisplay-dont-pause t
 )

;; other options
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(set-face-attribute 'default nil :height 140 :font "Inconsolata")

;; line numbers
(add-hook 'prog-mode-hook 'linum-mode)
(setq linum-format "%4d ")

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

;; ocodo-svg-modelines
(ocodo-svg-modelines-init)
(setq smt/current-theme 'ocodo-mesh-aqua-smt)

(provide 'init-ui)
;;; init-ui.el ends here
