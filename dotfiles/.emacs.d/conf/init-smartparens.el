;;; init-smartparens.el --- Configuration for smartparens
;;
;; Copyright (C) 2016 Brunno dos Santos <emacs at brunno dot me>
;;
;; Author: Brunno dos Santos @squiter
;; URL: http://github.com/squiter/emacs-dotfiles
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
(use-package smartparens
  :hook ((prog-mode . turn-on-smartparens-mode)
         (markdown-mode . turn-on-smartparens-mode))
  :bind (:map smartparens-mode-map
              ("C-c ( f" . sp-forward-sexp)
              ("C-c ( b" . sp-backward-sexp)
              ("C-c ( a" . sp-beginning-of-sexp)
              ("C-c ( e" . sp-end-of-sexp)
              ("C-c ( d" . sp-down-sexp)
              ("C-c ( u" . sp-up-sexp)
              ("C-c ( n" . sp-next-sexp)
              ("C-c ( p" . sp-previous-sexp)
              ("C-c ( k" . 'sp-kill-hybrid-sexp)
              ("C-c ( (" . 'hydra-smartparens/body)
              ("C-c ) s b" . 'sp-backward-slurp-sexp)
              ("C-c ) s f" . 'sp-forward-slurp-sexp)
              ("C-c ) b b" . 'sp-backward-barf-sexp)
              ("C-c ) b f" . 'sp-forward-barf-sexp)
              ("C-(" . 'sp-rewrap-sexp))
  :config

  ;; highlights matching pairs
  (show-smartparens-global-mode t)

  ;; Pipes are parens too :)
  (sp-with-modes '(ruby-mode)
    (sp-local-pair "|" "|"))

  ;; I don't know if this is really necessary, but it works
  (sp-with-modes '(elixir-mode)
    (sp-local-pair "do" "end"))

  (sp-with-modes '(org-mode)
    (sp-local-pair "=" "="))

  (sp-with-modes '(rhtml-mode)
    (sp-local-pair "<" ">")
    (sp-local-pair "<%" "%>")))

(provide 'init-smartparens)
;;; init-smartparens.el ends here
