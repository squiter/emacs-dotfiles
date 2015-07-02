(require 'evil)

(add-hook 'prog-mode-hook 'evil-local-mode)

;; change mode-line color by evil state
(lexical-let ((default-color (cons (face-background 'mode-line)
				   (face-foreground 'mode-line))))
  (add-hook 'post-command-hook
	    (lambda ()
	      (let ((color (cond ((minibufferp) default-color)
				 ((evil-insert-state-p) '("#AB4642" . "#E8E8E8"))
				 ((evil-emacs-state-p)  '("#444488" . "#E8E8E8"))
				 ((buffer-modified-p)   '("#006fa0" . "#E8E8E8"))
				 (t default-color))))
		(set-face-background 'mode-line (car color))
		(set-face-foreground 'mode-line (cdr color))))))

(require 'init-evil-surround)

(provide 'init-evil)
