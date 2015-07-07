(require 'smartparens)
(require 'smartparens-config)

(smartparens-global-mode t)

;; highlights matching pairs
(show-smartparens-global-mode t)

;; kill sexp
(define-key sp-keymap (kbd "C-M-k") 'sp-kill-hybrid-sexp)

;; navigation keybinds
(define-key sp-keymap (kbd "C-M-f") 'sp-forward-sexp)
(define-key sp-keymap (kbd "C-M-b") 'sp-backward-sexp)

;; change surround
(define-key sp-keymap (kbd "C-(") 'sp-rewrap-sexp)

;; Pipes are parens too :)
(sp-with-modes '(ruby-mode)
  (sp-local-pair "|" "|"))

(sp-with-modes '(rhtml-mode)
  (sp-local-pair "<" ">")
  (sp-local-pair "<%" "%>"))

(provide 'init-smartparens)
