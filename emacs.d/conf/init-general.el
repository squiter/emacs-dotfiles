;;; init-general.el --- My general configurations
;;; Commentary:
;;; Code:
(setq
 ;; default directory
 default-directory *projects-directory*
 backup-inhibited t
 ;; If a frame alredy opened, use it!
 display-buffer-reuse-frames t
 )

;; this require add support to dead-keys
(require 'iso-transl)

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

;; backups and autosaves
(defvar backup-dir (expand-file-name "~/.emacs.d/backups/"))
(defvar autosave-dir (expand-file-name "~/.emacs.d/autosave/"))
(setq backup-directory-alist (list (cons ".*" backup-dir)))
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))

(provide 'init-general)
;;; init-general.el ends here
