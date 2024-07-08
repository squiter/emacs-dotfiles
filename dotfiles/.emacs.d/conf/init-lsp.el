;;; init-lsp.el --- LSP Config File
;;
;; Copyright (C) 2020 Brunno dos Santos <emacs at brunno dot me>
;;
;; Author: Brunno dos Santos @squiter
;; URL: http://github.com/squiter/emacs-dotfiles
;;
;; Created: 27 April 2020
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

;; TODO: test this
(use-package lsp-mode
  :hook ((clojure-mode . lsp)
         (elixir-mode . lsp)
         (typescript-mode . lsp)
         (rustic-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration)
         (csharp-mode . lsp))
  :commands lsp
  :init
  (setq lsp-elixir-ls-version "v0.21.2")
  (setq lsp-keymap-prefix "C-c l")
  (setq gc-cons-threshold 100000000)
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (setq lsp-file-watch-threshold 1500)
  (setq lsp-lens-enable nil)
  (setq lsp-enable-folding t)

  ;; This function solves the `too many files open` problem at OSX
  ;; https://www.blogbyben.com/2022/05/gotcha-emacs-on-mac-os-too-many-files.html
  (defun file-notify-rm-all-watches ()
    "Remove all existing file notification watches from Emacs."
    (interactive)
    (maphash
     (lambda (key _value)
       (file-notify-rm-watch key))
     file-notify-descriptors))
  :config
  (add-to-list 'lsp-language-id-configuration '(nix-mode . "nix"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("rnix-lsp"))
                    :major-modes '(nix-mode)
                    :server-id 'nix))
  :custom
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-eldoc-render-all t))

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  (setq lsp-doc-mode nil
        lsp-ui-doc-position "top"
        lsp-ui-doc-show-with-cursor nil
        lsp-ui-flycheck-enable t
        lsp-ui-imenu-enable t
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-enable nil
        lsp-ui-peek-always-show t))

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)
(use-package lsp-origami
  :after lsp-mode
  :bind (("C-c l f c" . origami-close-node)
         ("C-c l f o" . origami-open-node)
         ("C-c l f a c" . origami-close-all-nodes)
         ("C-c l f a o" . origami-open-all-nodes))
  :init (add-hook 'lsp-after-open-hook #'lsp-origami-try-enable))

(provide 'init-lsp)
;;; init-lsp.el ends here
