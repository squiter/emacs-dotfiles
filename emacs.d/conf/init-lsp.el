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
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :init
  (setq lsp-keymap-prefix "C-c l"))

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  (setq lsp-ui-sideline-enable t
        lsp-ui-sideline-update-mode 'line
        lsp-ui-sideline-show-code-actions t
        lsp-ui-doc-position "top"
        lsp-ui-sideline-show-hover t
        lsp-ui-flycheck-enable t
        lsp-ui-imenu-enable t
        lsp-ui-sideline-ignore-duplicate t))
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)
(use-package company-lsp :commands company-lsp)

(provide 'init-lsp)
;;; init-lsp.el ends here
