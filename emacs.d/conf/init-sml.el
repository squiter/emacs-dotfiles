;;; init-sml.el --- Configurations to use SML language
;;; Commentary:
;;  This language was used in Programming Languages from Coursera
;;; Code:

;; custom configuration to use SML
(setenv "PATH" (concat "/usr/local/smlnj/bin:" (getenv "PATH")))
(setq exec-path (cons "/usr/local/smlnj/bin"  exec-path))

(provide 'init-sml)
;;; init-sml.el ends here
