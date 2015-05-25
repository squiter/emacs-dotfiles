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
(setq linum-format "%4d ")

;; Highlight current line - Disable because of my theme :(
;; (global-hl-line-mode 1)

;; Undo and Redo windows
(winner-mode 1)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; Open in fullscreen
(switch-fullscreen)

;; modeline
(defun branch-name ()
  (when vc-mode
    (concat "\ue0a0 " (substring vc-mode 5))
    ))

(defun current-ruby ()
  (when vc-mode
    (rbenv--active-ruby-version)
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
       '(:eval (current-ruby))
       "  |  "
       "%p (%l,%c)"
       ))

(provide 'init-ui)
