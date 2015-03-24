(setq
 ;; default directory
 default-directory (concat (getenv "HOME") "/projetos/")
 ;; disable backup files
 make-backup-files nil
 auto-save-default nil
 backup-inhibited t
 ;; make indentation commands use space only
 indent-tabs-mode nil
)

(provide 'init-general)
