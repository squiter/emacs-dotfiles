;;; init-simple-packages.el --- This file contains configurations for packages
;;; Commentary:
;;  When a package don't have enough configurations to have your own
;;  init-file I use this file
;;; Code:

(use-package exec-path-from-shell :ensure t :config (exec-path-from-shell-initialize))

(use-package avy
  :bind (("C-M-g c" . avy-goto-char)
         ("C-M-g 2" . avy-goto-char-2)
         ("C-M-g t" . avy-goto-char-timer)
         ("C-M-g w" . avy-goto-word-1)))

(use-package ag
  :commands (ag projectile-ag)
  :ensure-system-package ag)
(use-package rg
  :commands (rg projectile-ripgrep)
  :ensure-system-package rg)
(use-package indent-guide :config (indent-guide-global-mode))
(use-package neotree :bind ("C-c n" . 'neotree-toggle))
(use-package expand-region :bind ("C-=" . 'er/expand-region))
(use-package auto-package-update :config (auto-package-update-maybe))
(use-package undo-tree :defer t :config (global-undo-tree-mode 1))
;; TODO: try to use more this package:
(use-package smart-shift :config (global-smart-shift-mode 1))
(use-package beacon :config (beacon-mode 1))
(use-package docker :commands docker :ensure-system-package docker)
(use-package dockerfile-mode :mode "Dockerfile\\'")
(use-package bash-completion :defer t :config (bash-completion-setup))
(use-package indent-tools :commands indent-tools-hydra/body)
(use-package ssh-agency)
(use-package wgrep :defer t)
(use-package try :commands try)
(use-package free-keys :commands free-keys)
(use-package restart-emacs :commands restart-emacs)
(use-package git-timemachine :defer t)

(use-package multiple-cursors
  :commands (mc/mark-all-like-this
             mc/mark-more-like-this-extended
             mc/mark-more-like-this
             mc/edit-lines
             mc/set-rectangular-region-anchor)
  :init (setq mc/always-run-for-all t))

(use-package adjust-parens
  :hook (emacs-lisp-mode . adjust-parens-mode)
  :hook (clojure-mode . adjust-parens-mode))

(use-package init-java :ensure nil)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; edit-server
(use-package edit-server
  :config
  (when (require 'edit-server nil t)
    (setq edit-server-new-frame nil)
    (edit-server-start)))

;; all-the-icons
(use-package all-the-icons)
(use-package all-the-icons-dired
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package easy-kill
  :defer t
  :bind
  ([remap kill-ring-save] . easy-kill)
  ([remap mark-sexp] . easy-mark))

(use-package discover-my-major
  :bind ("C-h C-m" . discover-my-major))

(use-package imenu-list :commands imenu-list)
(use-package rainbow-mode)

(use-package beancount
  :ensure nil
  :mode ("\\.beancount\\'" . beancount-mode)
  :bind ("C-c h" . squiter/beancount-holdings)
  :config

  (defun squiter/beancount-holdings ()
    (interactive)
    (async-shell-command "bean-report ~/dropbox/ledger/ledger.beancount holdings")))


(use-package auto-highlight-symbol
  :hook (prog-mode . auto-highlight-symbol-mode))

(use-package speed-type :commands speed-type-text)

(use-package anzu
  :defer t
  :bind (("M-%" . 'anzu-query-replace)
         ("C-M-%" . 'anzu-query-replace-regexp)
         ("s-%" . 'anzu-query-replace-at-cursor)
         ("C-%" . 'anzu-replace-at-cursor-thing)))

(use-package symbol-overlay)

(use-package json-navigator
  :commands (json-navigator-navigate-after-point
             json-navigator-navigate-region))

;; Modes for highlight some files
(use-package i3wm-config-mode)
(use-package nix-mode :mode "\\.nix\\'")
(use-package scala-mode :interpreter ("scala" . scala-mode))
(use-package rhtml-mode :defer t)
(use-package terraform-mode :defer t)
(use-package lua-mode :defer t)
(use-package yaml-mode
  :mode (("\\.yml$" . yaml-mode)
         ("\\.yaml$" . yaml-mode)
         ("\\.yml\\.example$" . yaml-mode)))

(use-package tzc
  :init
  (if (file-exists-p "/run/current-system/etc/zoneinfo/")
      (setq tzc-main-dir "/run/current-system/etc/zoneinfo/"))
  (setq tzc-favourite-time-zones-alist '(("UTC+0000" "UTC")
                                         ("America/New_York" "New York")
                                         ("America/Sao_Paulo" "Sao Paulo")
                                         ("Europe/Lisbon"))))

(use-package string-inflection
  :bind ("C-q C-u" . 'my-string-inflection-cycle-auto)
  :init
  (global-unset-key (kbd "C-q"))

  :config
  (defun my-string-inflection-cycle-auto ()
    "switching by major-mode"
    (interactive)
    (cond
     ((eq major-mode 'python-mode)
      (string-inflection-python-style-cycle))
     ((eq major-mode 'java-mode)
      (string-inflection-java-style-cycle))
     ((eq major-mode 'elixir-mode)
      (string-inflection-elixir-style-cycle))
     ((eq major-mode 'ruby-mode)
      (string-inflection-ruby-style-cycle))
     (t
      (string-inflection-all-cycle)))))

(use-package csharp-mode
  :init
  (defun my/csharp-mode-hook ()
    (setq-local lsp-auto-guess-root t))
  (add-hook 'csharp-mode-hook #'my/csharp-mode-hook))

;; TODO: Remove this when unity.el was available on MELPA
;; (require 'unity)
;; (add-hook 'after-init-hook #'unity-mode)

(use-package request)

;; Themes
(use-package shades-of-purple-theme)
;; (use-package apropospriate-theme)
;; (use-package challenger-deep-theme)
;; (use-package doom-themes)

(use-package hl-todo :hook (prog-mode . hl-todo-mode))

(use-package fira-code-mode
  ;; :custom (fira-code-mode-disabled-ligatures '("[]" "x"))  ; ligatures you don't want
  :hook prog-mode)                                         ; mode to enable fira-code-mode in

(provide 'init-simple-packages)
;;; init-simple-packages.el ends here
