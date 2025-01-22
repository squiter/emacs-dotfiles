;;; early-init.el -*- lexical-binding: t; -*-

;; In Emacs 27+, package initialization occurs before `user-init-file' is
;; loaded, but after `early-init-file'. Doom handles package initialization, so
;; we must prevent Emacs from doing it early!
;; Copied from https://github.com/hlissner/doom-emacs/blame/develop/early-init.el

;;; Code:
(setq
 ;; better startup
 inhibit-splash-screen t
 inhibit-startup-message t
 ;; show column number at bottom bar
 column-number-mode t
 ;; disable anoying beep
 ring-bell-function 'ignore
 ;; improve rendering performance
 redisplay-dont-pause t
 )

;; other options
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; to fix the resize on OSX
(setq frame-resize-pixelwise t)

(add-to-list 'default-frame-alist '(undecorated-round . t))

(set-face-attribute 'default nil :height 160 :family "Fira Code")

;; disabling package.el to use Elpaca
(setq package-enable-at-startup nil)
