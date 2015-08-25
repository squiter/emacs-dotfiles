;;; init-ui.el --- Configurations for Emacs UI
;;; Commentary:
;;; Code:

;; theme options
(add-to-list 'custom-theme-load-path (concat *emacsd-directory* "/themes"))
(add-to-list 'load-path (concat *emacsd-directory* "/themes"))
(load-theme 'material t)

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
(scroll-bar-mode -1)
(set-face-attribute 'default nil :height 130)

;; line numbers
(add-hook 'prog-mode-hook 'linum-mode)
(setq linum-format "%4d ")

;; Highlight current line
(global-hl-line-mode 1)

;; Undo and Redo windows
(winner-mode 1)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; Open in fullscreen
(switch-fullscreen)

;; Transparency
(set-frame-parameter (selected-frame) 'alpha '(85 85))
(add-to-list 'default-frame-alist '(alpha 85 85))

(require 'init-telephone)

(provide 'init-ui)
;;; init-ui.el ends here
