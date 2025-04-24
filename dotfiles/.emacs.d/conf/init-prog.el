;;; init-prog.el --- Programming Stuff
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
;; I'm trying to keep stuff simpler in this Emacs configuration, so
;; I'll try to keep all my programming stuff in a single file

;;; Code:

;; lisp stuff
(use-package adjust-parens
  :hook (emacs-lisp-mode . adjust-parens-mode)
  :hook (clojure-mode . adjust-parens-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package emacs
  :ensure nil
  :custom

  ;; Should use:
  ;; (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))
  ;; at least once per installation or while changing this list
  (treesit-language-source-alist
   '((heex "https://github.com/phoenixframework/tree-sitter-heex")
     (elixir "https://github.com/elixir-lang/tree-sitter-elixir")))

  (major-mode-remap-alist
   '((elixir-mode . elixir-ts-mode))))

(use-package eglot
 :ensure nil
 :config (add-to-list 'eglot-server-programs '(elixir-ts-mode "language_server.sh")))

(use-package elixir-ts-mode
 :hook (elixir-ts-mode . eglot-ensure)
 (elixir-ts-mode
  .
  (lambda ()
    (push '(">=" . ?\u2265) prettify-symbols-alist)
    (push '("<=" . ?\u2264) prettify-symbols-alist)
    (push '("!=" . ?\u2260) prettify-symbols-alist)
    (push '("==" . ?\u2A75) prettify-symbols-alist)
    (push '("=~" . ?\u2245) prettify-symbols-alist)
    (push '("<-" . ?\u2190) prettify-symbols-alist)
    (push '("->" . ?\u2192) prettify-symbols-alist)
    (push '("<-" . ?\u2190) prettify-symbols-alist)
    (push '("|>" . ?\u25B7) prettify-symbols-alist)))
 (before-save . eglot-format))

(use-package exunit
  :hook (elixir-ts-mode . exunit-mode)

  :bind
  (:map elixir-ts-mode-map
        ("C-c , a" . exunit-verify-all)
        ("C-c , A" . exunit-verify-all-in-umbrella)
        ("C-c , s" . exunit-verify-single)
        ("C-c , v" . exunit-verify)
        ("C-c , r" . exunit-rerun)
        ("C-c , f" . exunit-toggle-file-and-test)))

(use-package fish-mode)

(use-package auto-highlight-symbol :hook (prog-mode . auto-highlight-symbol-mode))

(use-package aider
  :ensure (:host github :repo "tninja/aider.el")
  :config
  ;; For latest claude sonnet model
  (setopt aider-args '("--model" "sonnet" "--no-auto-accept-architect"))
  (setenv "ANTHROPIC_API_KEY" (getenv "ANTHROPIC_API_KEY"))
  ;; Or chatgpt model
  ;; (setq aider-args '("--model" "o4-mini"))
  ;; (setenv "OPENAI_API_KEY" <your-openai-api-key>)
  ;; Or gemini model
  ;; (setq aider-args '("--model" "gemini-exp"))
  ;; (setenv "GEMINI_API_KEY" <your-gemini-api-key>)
  ;; Or use your personal config file
  (setopt aider-args `("–no-auto-commits"))
  (setopt aider--switch-to-buffer-other-frame nil)
  ;; ;;
  ;; Optional: Set a key binding for the transient menu
  (global-set-key (kbd "C-c a") 'aider-transient-menu))

(provide 'init-prog)
;; init-prog.el ends here
