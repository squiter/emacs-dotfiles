;;; init-ivy.el --- Ivy configuration
;;
;; Copyright (C) 2016 Brunno dos Santos <emacs at brunno dot me>
;;
;; Author: Brunno dos Santos @squiter
;; URL: http://github.com/squiter/emacs-dotfiles
;;
;; Created: 13 outubro 2016
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

;;; Code:
(use-package counsel
  :after ivy
  :config
  (counsel-mode)

  (setq ivy-initial-inputs-alist nil)

  (defun ivy-with-thing-at-point (cmd)
    (let ((ivy-initial-inputs-alist
           (list
            (cons cmd (thing-at-point 'symbol)))))
      (funcall cmd)))

  ;; Example 1
  (defun counsel-ag-thing-at-point ()
    (interactive)
    (ivy-with-thing-at-point 'counsel-ag))

  ;; Example 2
  (defun swiper-thing-at-point ()
    (interactive)
    (ivy-with-thing-at-point 'swiper))

  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-x C-o" . counsel-org-goto-all)
         ("<f1> f" . counsel-describe-function)
         ("<f1> v" . counsel-describe-variable)
         ("<f1> l" . counsel-load-library)
         ("<f2> i" . counsel-info-lookup-symbol)
         ("<f2> u" . counsel-unicode-char)
         ("C-x C-l" . counsel-locate)
         ("M-n b b" . counsel-bookmarks)
         ("C-S-y" . counsel-yank-pop)
         ("C-M-y" . counsel-yank-pop)
         ("M-n a" . counsel-ag-thing-at-point)
         ("M-n s" . swiper-thing-at-point)))

(use-package ivy
  :defer 0.1
  :diminish
  :bind (("C-c C-r" . ivy-resume)
         ("C-x B" . ivy-switch-buffer-other-window)
         ("C-c v" . ivy-push-view)
         ("C-c V" . ivy-pop-view)
         ("C-x b" . ivy-switch-buffer)
         ("C-!" . ivy-yank-complete-symbol-at-point))
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  (ivy-height 15)
  (ivy-initial-inputs-alist nil)
  :config

  (ivy-mode)

  (defun ivy-yank-complete-symbol-at-point (&optional arg)
  "inserts whole symbol from buffer to ivy prompt. Prefix args allowed"
  (interactive "p")
  (unless (fboundp 'forward-symbol)
    (require 'thingatpt))
  (let ((text (with-ivy-window
      		(forward-thing 'symbol (or arg 1))
      		(thing-at-point 'symbol 'no-props))))
    (when text
      (insert (replace-regexp-in-string " +" " " text t t))))))

(use-package ivy-rich
  :after ivy
  :custom
  (ivy-virtual-abbreviate 'full
                          ivy-rich-switch-buffer-align-virtual-buffer t
                          ivy-rich-path-style 'abbrev)
  :config (ivy-rich-mode 1))

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

;; TODO: Make it works with ivy-rich
;; (use-package all-the-icons-ivy
;;   :after ivy all-the-icons
;;   :config (all-the-icons-ivy-setup))

(provide 'init-ivy)
;;; init-ivy.el ends here

