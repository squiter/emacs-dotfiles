;;; init-keybindings.el --- My custom keybindigs
;;; Commentary:
;;  This file doesn't contain specific mode keybindings
;;; Code:

;; C-c prefix
(global-set-key (kbd "C-c d") 'custom/duplicate-current-line-or-region)
(global-set-key (kbd "C-c /") 'custom/toggle-line-comment)
(global-set-key (kbd "C-c j") 'join-line)
(global-set-key (kbd "C-c w") 'whitespace-cleanup)
(global-set-key (kbd "C-c i") 'indent-buffer)
(global-set-key (kbd "C-c e") 'eval-buffer)

;; C-x prefix
(global-set-key (kbd "C-x -") 'hsplit-last-buffer)
(global-set-key (kbd "C-x |") 'vsplit-last-buffer)
(global-set-key (kbd "C-x =") 'swap-buffers-in-windows)
(global-set-key (kbd "C-x C-S-k") 'delete-current-buffer-file)
(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)
(global-set-key (kbd "C-x !") 'sudo-edit)

;; Other prefix
(global-set-key (kbd "C-h C-m") 'discover-my-major)
(global-set-key (kbd "C-<return>") 'custom/insert-new-line)
(global-set-key (kbd "C-a") 'custom/smart-move-beginning-of-line)

;; Kill Ring
;; TODO: Chose one of those keybinds
(global-set-key (kbd "C-S-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-M-y") 'helm-show-kill-ring)

;; window and buffer manipulation
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-i") 'other-frame)
(global-set-key (kbd "M-0") 'delete-window)
(global-set-key (kbd "M-k") 'kill-buffer)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-4") 'delete-other-windows)
(global-set-key (kbd "M-5") 'delete-window)

;; bookmark-bidings
(global-set-key (kbd "s-b s") 'bookmark-set)
(global-set-key (kbd "s-b l") 'bookmark-bmenu-list)
(global-set-key (kbd "s-b j") 'bookmark-jump)
(global-set-key (kbd "s-b o j") 'bookmark-jump-other-window)

(global-unset-key (kbd "C-z"))

;;======================================================================;;
;;======================  Package's Keybinds ===========================;;
;;======================================================================;;

;; avy
(global-set-key (kbd "C-x a f") 'avy-goto-line)
(global-set-key (kbd "C-x a w") 'avy-goto-word-1)
(global-set-key (kbd "C-x a e") 'avy-goto-word-0)

;; ace-window
(global-set-key (kbd "C-x a W") 'ace-window)

;; highlight-symbol
(global-set-key [(super f3)] 'highlight-symbol)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
(global-set-key [(meta f3)] 'highlight-symbol-query-replace)

;; easy-kill
(global-set-key [remap kill-ring-save] 'easy-kill)
(global-set-key [remap mark-sexp] 'easy-mark)

(provide 'init-keybindings)
;;; init-keybindings.el ends here
