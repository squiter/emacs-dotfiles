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
(global-set-key (kbd "C-c C-/") 'custom/toggle-line-comment)
(global-set-key (kbd "C-c C-y") 'custom/yank-and-indent)
(global-set-key (kbd "C-c M-c") 'custom/chomp)
(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)
(global-set-key [remap fill-paragraph] #'endless/fill-or-unfill)
(define-key ctl-x-map "n" #'narrow-or-widen-dwim)

;; miscellaneous
(global-set-key (kbd "C-c w") 'whitespace-cleanup)
(global-set-key (kbd "C-c i") 'indent-buffer)
(global-set-key (kbd "C-c e") 'eval-buffer)
(global-set-key (kbd "C-c ]") 'custom/org-open-project-file)

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
(global-set-key (kbd "M-n b b") 'helm-bookmarks)

;; unseted keybinds
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
(global-set-key (kbd "C-S-m m") 'livedown:preview)

;; undo-tree
(global-set-key (kbd "C-z") 'undo-tree-undo)
(global-set-key (kbd "C-S-z") 'undo-tree-redo)

;; zeal-at-point
(global-set-key (kbd "C-c C-d") 'zeal-at-point)

;; yasnippet
(global-set-key (kbd "C-c y n") 'yas-new-snippet)
(global-set-key (kbd "C-c y d") 'custom/yas-dired)

;; multiple-cursors
(global-set-key (kbd "C-c m l") 'mc/edit-lines)
(global-set-key (kbd "C-c m m") 'mc/mark-more-like-this-extended)
(global-set-key (kbd "C-c m a") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c m r") 'mc/set-rectangular-region-anchor)

(global-set-key (kbd "C-c f") 'aj-toggle-fold)

;; kill sexp
(define-key smartparens-mode-map (kbd "M-[ k") 'sp-kill-hybrid-sexp)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Smartparend keybinds ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; navigation keybinds
(define-key smartparens-mode-map (kbd "M-[ f") 'sp-forward-sexp)
(define-key smartparens-mode-map (kbd "M-[ b") 'sp-backward-sexp)
(define-key smartparens-mode-map (kbd "M-[ a") 'sp-beginning-of-sexp)
(define-key smartparens-mode-map (kbd "M-[ e") 'sp-end-of-sexp)
(define-key smartparens-mode-map (kbd "M-[ d") 'sp-down-sexp)
(define-key smartparens-mode-map (kbd "M-[ u") 'sp-up-sexp)
(define-key smartparens-mode-map (kbd "M-[ n") 'sp-next-sexp)
(define-key smartparens-mode-map (kbd "M-[ p") 'sp-previous-sexp)

;; slurp and barf
(define-key smartparens-mode-map (kbd "M-] s b") 'sp-backward-slurp-sexp)
(define-key smartparens-mode-map (kbd "M-] s f") 'sp-forward-slurp-sexp)
(define-key smartparens-mode-map (kbd "M-] b b") 'sp-backward-barf-sexp)
(define-key smartparens-mode-map (kbd "M-] b f") 'sp-forward-barf-sexp)

;; change surround
(define-key smartparens-mode-map (kbd "C-(") 'sp-rewrap-sexp)

(provide 'init-keybindings)
;;; init-keybindings.el ends here
