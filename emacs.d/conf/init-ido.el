(use-package ido-vertical-mode
  :init
  (ido-mode 1)
  (ido-vertical-mode 1)
  (setq
   ;; Set C-n and C-p to work
   ido-vertical-define-keys 'C-n-and-C-p-only
   ;; Don’t be case sensitive
   ido-case-fold t))

(provide 'init-ido)
