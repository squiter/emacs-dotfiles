;;; init-simple-packages.el --- This file contains configurations for packages
;;; Commentary:
;;  When a package don't have enough configurations to have your own
;;  init-file I use this file
;;; Code:

;; Requiring Apel alist
(require 'alist)

;; anzu
(global-anzu-mode +1)

;; Indent-guide
(indent-guide-global-mode)

;; back-button
(back-button-mode 1)

;; neotree
(global-set-key (kbd "C-c n") 'neotree-toggle)

;; expand-region
(global-set-key (kbd "C-=") 'er/expand-region)

;; auto-package-update
(auto-package-update-maybe)

;; yagist
(setq yagist-github-token *user-github-token*)

;; typing of emacs
(autoload 'typing-of-emacs "typing" "The Typing Of Emacs, a game." t)

;; rainbow-delimiters
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; require vmd-mode
(require 'vmd-mode)

;; turn on undo-tree everywhere
(global-undo-tree-mode 1)

;; multiple cursors
(require 'multiple-cursors)
(setq mc/always-run-for-all t)

;; smart-shift
(global-smart-shift-mode 1)

;; beacon-mode
(beacon-mode 1)

;; indent-tools
(require 'indent-tools)

;; ivy-rich
(require 'ivy-rich)
(ivy-set-display-transformer 'ivy-switch-buffer 'ivy-rich-switch-buffer-transformer)

;; all-the-icons
(require 'all-the-icons)
(all-the-icons-ivy-setup)
(add-hook 'dired-mode-hook #'all-the-icons-dired-mode)

;; dired-collapse
(require 'dired-collapse)
(add-hook 'dired-mode-hook #'dired-collapse-mode)

;; calfw
(require 'calfw)
(require 'calfw-org)

;; bash-completion
(require 'bash-completion)
(bash-completion-setup)

;; pdf-tools
(pdf-tools-install)

(provide 'init-simple-packages)
;;; init-simple-packages.el ends here
