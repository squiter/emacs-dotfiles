;;; init-keybindings.el --- My custom keybindigs
;;; Commentary:
;;  This file doesn't contain specific mode keybindings
;;; Code:

;; init-edit-custom-functions.el keybinds:
(global-set-key (kbd "C-<return>") 'custom/insert-new-line)
(global-set-key [remap move-beginning-of-line] #'custom/smart-move-beginning-of-line)
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

;; window and buffer manipulation
(global-set-key (kbd "C-x |") 'vsplit-last-buffer)
(global-set-key (kbd "C-x -") 'hsplit-last-buffer)
(global-set-key (kbd "C-x =") 'swap-buffers-in-windows)
(global-set-key (kbd "M-o") 'other-window)
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
(global-unset-key (kbd "C-x C-z"))

;;======================================================================;;
;;======================  Package's Keybinds ===========================;;
;;======================================================================;;

;; sexp navigation
(global-set-key [(super f)] 'sp-next-sexp)
(global-set-key [(super b)] 'sp-previous-sexp)

;; swiper
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-r") 'swiper)
(global-set-key (kbd "C-c u") 'swiper-all)

;; language tools
;; I'm overwriting (count-lines-page) keybind!
(global-unset-key (kbd "C-x l"))
(global-set-key (kbd "C-x l c") 'langtool-check)
(global-set-key (kbd "C-x l d") 'langtool-check-done)
(global-set-key (kbd "C-x l s") 'langtool-switch-default-language)
(global-set-key (kbd "C-x l m") 'langtool-show-message-at-point)
(global-set-key (kbd "C-x l b") 'langtool-correct-buffer)

;; undo-tree
(global-set-key (kbd "C-z") 'undo-tree-undo)
(global-set-key (kbd "C-S-z") 'undo-tree-redo)

;; yasnippet
(global-set-key (kbd "C-c y n") 'yas-new-snippet)
(global-set-key (kbd "C-c y d") 'custom/yas-dired)

;; multiple-cursors
(global-set-key (kbd "C-c m l") 'mc/edit-lines)
(global-set-key (kbd "C-c m m") 'mc/mark-more-like-this-extended)
(global-set-key (kbd "C-c m a") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c m n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c m r") 'mc/set-rectangular-region-anchor)

;; ctags stuff
(global-set-key (kbd "M-*") 'pop-tag-mark)

;; Indent-tools hydra
(global-set-key (kbd "C-c >") 'indent-tools-hydra/body)


;;;;;;;;;;;;;;;
;; ruby-mode ;;
;;;;;;;;;;;;;;;

(eval-after-load 'enh-ruby-mode
  '(progn
     (define-key enh-ruby-mode-map (kbd "C-c , ,") 'senny-ruby-open-spec-other-buffer)
     (define-key enh-ruby-mode-map (kbd "#") 'senny-ruby-interpolate)
     (define-key enh-ruby-mode-map (kbd "C-, b e") 'bundle-exec)
     (define-key enh-ruby-mode-map (kbd "C-, b i") 'bundle-install)
     (define-key enh-ruby-mode-map (kbd "C-, b o") 'bundle-open)
     (define-key enh-ruby-mode-map (kbd "C-, b c") 'bundle-console)
     (define-key enh-ruby-mode-map (kbd "C-c v") 'custom/vcr-toggle)
     (define-key enh-ruby-mode-map (kbd "M-n s") 'projectile-rails-server)
     (define-key enh-ruby-mode-map (kbd "M-n c") 'projectile-rails-console)
     (define-key enh-ruby-mode-map (kbd "C-, l m") 'squiter/rails-go-to-last-migration)
     (define-key enh-ruby-mode-map (kbd "C-c M-j") #'endless/run-ruby)
     (define-key enh-ruby-mode-map (kbd "M-n l") 'squiter/ruby-linear-modules)))

;;;;;;;;;;
;; Git  ;;
;;;;;;;;;;

;; To resolve git conflicts
(setq smerge-command-prefix (kbd "C-c M-s"))


;;;;;;;;;;;;;;;;;;;;
;; symbol-overlay ;;
;;;;;;;;;;;;;;;;;;;;
(global-unset-key (kbd "M-i"))
(global-set-key (kbd "M-i i") 'symbol-overlay-put)
(global-set-key (kbd "M-i n") 'symbol-overlay-switch-forward)
(global-set-key (kbd "M-i p") 'symbol-overlay-switch-backward)
(global-set-key (kbd "M-i r") 'symbol-overlay-rename)
(global-set-key (kbd "M-i d") 'symbol-overlay-jump-to-definition)
(global-set-key (kbd "M-i <f7>") 'symbol-overlay-mode)
(global-set-key (kbd "M-i <f8>") 'symbol-overlay-remove-all)

(define-key edit-server-edit-mode-map (kbd "C-c C-c") 'edit-server-done)

(global-set-key (kbd "C-c r") 'raise-sexp)

(provide 'init-keybindings)
;;; init-keybindings.el ends here
