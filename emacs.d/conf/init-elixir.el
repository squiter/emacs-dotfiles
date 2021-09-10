;;; init-elixir.el --- Elixir configurations
;;
;; Copyright (C) 2017 Brunno dos Santos <emacs at brunno dot me>
;;
;; Author: Brunno dos Santos @squiter
;; URL: http://github.com/squiter/emacs-dotfiles
;;
;; Created: 20 julho 2017
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
(use-package alchemist
  :hook ((elixir-mode . alchemist-mode)
         (elixir-mode . flycheck-mode))
  :bind (:map elixir-mode-map
              ("C-c i" . elixir-format))
  :init (setq alchemist-key-command-prefix (kbd "C-c ,")))

(use-package flycheck-credo
  :after alchemist
  :config (eval-after-load 'flycheck '(flycheck-credo-setup)))

(use-package exunit
  :hook (elixir-mode . exunit-mode))

(provide 'init-elixir)
;;; init-elixir.el ends here
