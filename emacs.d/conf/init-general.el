;;; init-general.el --- My general configurations
;;; Commentary:
;;; Code:
(setq
 ;; default directory
 default-directory (concat (getenv "HOME") "/projetos/")
 ;; disable backup files
 make-backup-files nil
 auto-save-default nil
 backup-inhibited t
 ;; If a frame alredy opened, use it!
 display-buffer-reuse-frames t
)

;; make indentation commands use space only
(setq-default indent-tabs-mode nil)

;; dired configurations
(put 'dired-find-file-other-buffer 'disabled t)

;; whitespace display
(global-whitespace-mode)
(setq whitespace-global-modes
      '(not magit-mode git-commit-mode))
(setq whitespace-style '(face trailing tabs))

(provide 'init-general)
;;; init-general.el ends here
