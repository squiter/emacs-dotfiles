;;; init-general.el --- My general configurations
;;; Commentary:
;;; Code:
(setq
 ;; default directory
 default-directory *projects-directory*
 ;; disable backup files
 make-backup-files nil
 auto-save-default nil
 backup-inhibited t
 ;; If a frame alredy opened, use it!
 display-buffer-reuse-frames t
)

;; replace marked text when type
(delete-selection-mode 1)

;; Customize BS
(setq custom-file (concat *emacsd-directory* "/custom.el"))
(load custom-file)

;; move cursor by camelCase
(subword-mode 1)

;; make indentation commands use space only
(setq-default indent-tabs-mode nil)

;; whitespace display
(global-whitespace-mode)
(setq whitespace-global-modes
      '(not magit-mode git-commit-mode))
(setq whitespace-style '(face trailing tabs))

(provide 'init-general)
;;; init-general.el ends here
