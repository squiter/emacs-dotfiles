(eval-after-load 'ruby-mode
  '(progn
     (global-set-key (kbd "C-c r c") 'rinari-console)
     (global-set-key (kbd "C-c r s") 'rinari-web-server)))

(provide 'init-rinari)
