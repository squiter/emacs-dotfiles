;;; init-mac-switch-meta.el --- Configures Emacs Mac Port met key-binding
;;; Commentary:
;;  This port of Emacs uses command as meta key, in this file we add a function
;;  to switch between command and alt keys
;;; Code:

;; Keybinds
(global-set-key [(super a)] 'mark-whole-buffer)
(global-set-key [(super v)] 'yank)
(global-set-key [(super c)] 'kill-ring-save)
(global-set-key [(super s)] 'save-buffer)
(global-set-key [(super l)] 'goto-line)
(global-set-key [(super w)]
                (lambda () (interactive) (delete-window)))
(global-set-key [(super z)] 'undo)
(global-set-key [(super k)] 'kill-this-buffer)
(global-set-key [(super u)] 'revert-buffer)

;; Set alt as meta key as default
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)

;; mac switch meta key
(defun mac-switch-meta nil
  "Switch meta between Option and Command."
  (interactive)
  (if (eq mac-option-modifier nil)
      (progn
	(setq mac-option-modifier 'meta)
	(setq mac-command-modifier 'super)
	)
    (progn
      (setq mac-option-modifier nil)
      (setq mac-command-modifier 'meta)
      )
    )
  )

(provide 'init-mac-switch-meta)
;;; init-mac-switch-meta.el ends here
