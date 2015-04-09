;; theme options
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'darktooth t)

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
(set-face-attribute 'default nil :height 140)

;; line numbers
(add-hook 'prog-mode-hook 'linum-mode)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; Open in fullscreen
(switch-fullscreen)

;; modeline
(defun branch-name ()
  (when vc-mode
    (concat "\ue0a0 " (substring vc-mode 5))
    ))

(setq-default mode-line-format
      (list
       "[" mode-line-modified "]"
       "  "
       "%b"
       "  |  "
       'mode-name
       "  |  "
       '(:eval (projectile-project-name))
       " "
       '(:eval (branch-name))
       "  |  "
       "%p (%l,%c)"
       ))

(provide 'init-ui)
