;;; init-erc.el --- My ERC configurations
;;
;; Copyright (C) 2015 Brunno dos Santos <emacs at brunno dot me>
;;
;; Author: Brunno dos Santos @squiter
;; URL: http://github.com/squiter/dotfiles
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; See <http://www.gnu.org/licenses/> for a copy of the GNU General
;; Public License.
;;
;;; Commentary:
;;
;; Most part of this file was stolen from Milhouse @
;; https://github.com/rranelli/emacs-dotfiles/

;;; Code:
(require 'erc)
(require 'erc-join)
(require 'erc-match)
(require 'erc-track)
(require 'erc-fill)
(require 'erc-ring)
(require 'erc-netsplit)

(require 'erc-hl-nicks)
(add-to-list 'erc-modules 'hl-nicks)
(erc-update-modules)

(require 'erc-image)
(add-to-list 'erc-modules 'image)
(erc-update-modules)

(erc-hl-nicks-mode t)
(erc-notifications-mode t)
(erc-match-mode t)
(erc-autojoin-mode t)
(erc-track-mode t)
(erc-fill-mode t)
(erc-ring-mode t)
(erc-netsplit-mode t)
(erc-timestamp-mode t)
(erc-button-mode (- 1))

(setq erc-track-exclude-types '("MODE" "AWAY" "JOIN" "PART")
      erc-track-use-faces t
      erc-hide-list '("JOIN" "PART" "QUIT" "AWAY")
      erc-max-buffer-size 20000
      erc-auto-query 'bury
      erc-query-display 'buffer
      erc-query-display 'buffer
      erc-log-insert-log-on-open t
      erc-log-channels t
      erc-log-channels-directory "~/.irclogs/"
      erc-save-buffer-on-part t
      erc-kill-buffer-on-part t)

(setq erc-autojoin-channels-alist
      '(("freenode" "#haskell" "haskell-emacs" "#emacs" "#ruby-lang")))

(defun rr/irc-freenode ()
  "Connect to freenode IRC."
  (interactive)
  (erc-tls :server "irc.freenode.net"
           :port 6697
           :nick "squiter"
           :full-name "freenode@brunno.me"))

(defun rr/join-irc ()
  "Connect to all irc servers."
  (rr/irc-freenode))

(defun rr/clear-erc-unseen ()
  "Clears irc modified channels notification."
  (interactive)
  (setq erc-modified-channels-alist nil)
  (erc-modified-channels-display))

;;
;;; keybindings
;;
(global-set-key (kbd "C-c C-u") 'rr/clear-erc-unseen)

(define-key erc-mode-map (kbd "C-x C-s") 'ignore)
(define-key erc-mode-map (kbd "C-l") '(lambda () (interactive) (erc-cmd-CLEAR)))

(provide 'init-erc)
;;; init-erc.el ends here
