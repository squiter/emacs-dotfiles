;;; init-keybindings.el --- My custom keybindigs
;;; Commentary:
;;  This file doesn't contain specific mode keybindings
;;; Code:

;; init-edit-custom-functions.el keybinds:
(global-set-key (kbd "C-<return>") 'custom/insert-new-line)
(global-set-key [remap move-beginning-of-line]
                #'custom/smart-move-beginning-of-line)
(global-set-key (kbd "C-c d") 'custom/duplicate-current-line-or-region)
(global-set-key (kbd "C-c M-w") 'custom/copy-line)
(global-set-key [remap kill-whole-line] #'custom/kill-line)
(global-set-key (kbd "C-c j") 'custom/join-line)
(global-set-key (kbd "C-c /") 'custom/toggle-line-comment)
(global-set-key (kbd "C-c C-y") 'custom/yank-and-indent)
(global-set-key (kbd "C-c M-c") 'custom/chomp)
(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)
(global-set-key [remap fill-paragraph] #'endless/fill-or-unfill)

;; miscellaneous
(global-set-key (kbd "C-c w") 'whitespace-cleanup)
(global-set-key (kbd "C-c i") 'indent-buffer)
(global-set-key (kbd "C-c e") 'eval-buffer)

;; buffer/file edits
(global-set-key (kbd "C-x C-S-k") 'delete-current-buffer-file)
(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)
(global-set-key (kbd "C-x !") 'sudo-edit)

;; Kill Ring
;; TODO: Chose one of those keybinds
(global-set-key (kbd "C-S-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-M-y") 'helm-show-kill-ring)

;; window and buffer manipulation
(global-set-key (kbd "C-x |") 'vsplit-last-buffer)
(global-set-key (kbd "C-x -") 'hsplit-last-buffer)
(global-set-key (kbd "C-x =") 'swap-buffers-in-windows)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-i") 'other-frame)
(global-set-key (kbd "M-0") 'delete-window)
(global-set-key (kbd "M-k") 'kill-buffer)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-4") 'delete-other-windows)
(global-set-key (kbd "M-5") 'delete-window)

;; bookmark-bidings
(global-set-key (kbd "M-n b s") 'bookmark-set)
(global-set-key (kbd "M-n b l") 'bookmark-bmenu-list)
(global-set-key (kbd "M-n b j") 'bookmark-jump)
(global-set-key (kbd "M-n b o") 'bookmark-jump-other-window)

;; unseted keybinds
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

;;======================================================================;;
;;======================  Package's Keybinds ===========================;;
;;======================================================================;;

;; anzu
(global-set-key (kbd "M-%") 'anzu-query-replace)
(global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)
(global-set-key (kbd "s-%") 'anzu-query-replace-at-cursor)
(global-set-key (kbd "C-%") 'anzu-replace-at-cursor-thing)

;; avy
(global-set-key (kbd "C-x a f") 'avy-goto-line)
(global-set-key (kbd "C-x a w") 'avy-goto-word-1)
(global-set-key (kbd "C-x a e") 'avy-goto-word-0)

;; ace-window
(global-set-key (kbd "C-x a W") 'ace-window)

;; hl-anything
(global-set-key [(super f3)] 'hl-highlight-thingatpt-local)
(global-set-key [f3] 'hl-find-next-thing)
(global-set-key [(shift f3)] 'hl-find-prev-thing)

;; easy-kill
(global-set-key [remap kill-ring-save] 'easy-kill)
(global-set-key [remap mark-sexp] 'easy-mark)

;; sexp navigation
(global-set-key [(super f)] 'sp-next-sexp)
(global-set-key [(super b)] 'sp-previous-sexp)

;; discover my major
(global-set-key (kbd "C-h C-m") 'discover-my-major)

;; swiper
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-r") 'swiper)
(global-set-key (kbd "C-c u") 'swiper-all)
(global-set-key (kbd "C-c C-r") 'helm-resume)

;; language tools
;; I'm overwriting (count-lines-page) keybind!
(global-unset-key (kbd "C-x l"))
(global-set-key (kbd "C-x l c") 'langtool-check)
(global-set-key (kbd "C-x l d") 'langtool-check-done)
(global-set-key (kbd "C-x l s") 'langtool-switch-default-language)
(global-set-key (kbd "C-x l m") 'langtool-show-message-at-point)
(global-set-key (kbd "C-x l b") 'langtool-correct-buffer)

;; google-this and google-translate
(global-set-key (kbd "C-x g") 'google-this-mode-submap)
(global-set-key [remap google-this] 'google-translate-smooth-translate)

;; emacs-livedown
(global-set-key (kbd "C-M m") 'livedown:preview)

(provide 'init-keybindings)
;;; init-keybindings.el ends here
