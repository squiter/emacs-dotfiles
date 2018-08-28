;;; init-simple-packages.el --- This file contains configurations for packages
;;; Commentary:
;;  When a package don't have enough configurations to have your own
;;  init-file I use this file
;;; Code:

(use-package ag)
(use-package anzu :config (global-anzu-mode +1))
(use-package indent-guide :config (indent-guide-global-mode))
(use-package back-button :config (back-button-mode 1))
(use-package neotree :bind ("C-c n" . 'neotree-toggle))
(use-package expand-region :bind ("C-=" . 'er/expand-region))
(use-package auto-package-update :config (auto-package-update-maybe))
(use-package undo-tree :config (global-undo-tree-mode 1))
(use-package smart-shift :config (global-smart-shift-mode 1))
(use-package beacon :config (beacon-mode 1))
(use-package multiple-cursors :init (setq mc/always-run-for-all t))
(use-package beancount :mode ("\\.beancount\\'" . beancount-mode))
(use-package dockerfile-mode :mode "Dockerfile\\'")
(use-package bash-completion :config (bash-completion-setup))
(use-package pdf-tools :config (pdf-tools-install))
(use-package magithub :config (magithub-feature-autoinject t))
(use-package dired-collapse :hook dired-mode)
(use-package vmd-mode)
(use-package indent-tools)
(use-package ssh-agency)
(use-package jira-markup-mode :ensure t)
(use-package cheat-sh)
(use-package wgrep)
(use-package try)
(use-package free-keys)
(use-package restart-emacs)
(use-package zeal-at-point :bind ("C-c C-d" . 'zeal-at-point))
(use-package pocket-reader)

(use-package init-calendars
  :ensure nil ;; "package" created with config/init-calendars.el
  :config

  (defun cfw:open-all-calendars ()
    (interactive)
    (cfw:open-calendar-buffer
     :contents-sources
     (list
      (cfw:org-create-source "Green")
      (cfw:ical-create-source "Locaweb" *locaweb-ical-url* "IndianRed")
      (cfw:ical-create-source "Google" *google-principal-calendar-url* "Red")))))

(use-package init-java :ensure nil)

(use-package yagist
  :init
  (setq yagist-github-token *user-github-token*))

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

(provide 'init-simple-packages)
;;; init-simple-packages.el ends here
