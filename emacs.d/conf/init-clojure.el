;;; init-clojure.el --- Clojure configurations
;;
;; Copyright (C) 2015 Brunno dos Santos <emacs at brunno dot me>
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
;; I'm reading Clojure for the Brave and True and I started this file
;; using this Emacs configuration:
;; https://github.com/flyingmachine/emacs-for-clojure/

;;; Code:
(use-package clojure-mode
  :hook (clojure-mode . subword-mode)
  :hook (clojure-mode . (lambda ()
                          (setq inferior-lisp-program "lein repl")
                          (font-lock-add-keywords
                           nil
                           '(("(\\(facts?\\)"
                              (1 font-lock-keyword-face))
                             ("(\\(background?\\)"
                              (1 font-lock-keyword-face))))
                          (define-clojure-indent (fact 1))
                          (define-clojure-indent (facts 1))))

  :bind (:map clojure-mode-map
              ("C-c C-r m l" . clojure-move-to-let))

  :init
  (eval-after-load 'clojure
    (define-key clojure-mode-map (kbd "M-n n") 'clojure-insert-ns-form))

  :config
  (define-clojure-indent
    (defroutes 'defun)
    (GET 2)
    (POST 2)
    (PUT 2)
    (DELETE 2)
    (HEAD 2)
    (ANY 2)
    (OPTIONS 2)
    (PATCH 2)
    (rfn 2)
    (let-routes 1)
    (context 2)))

(use-package clojure-mode-extra-font-locking)

(use-package cider
  :mode (("\\.edn$" . clojure-mode)
         ("\\.boot$" . clojure-mode)
         ("\\.cljs.*$" . clojure-mode)
         ("lein-env" . enh-ruby-mode))
  :hook (cider-mode . cider-turn-on-eldoc-mode)
  :config
  ;; go right to the REPL buffer when it's finished connecting
  (setq cider-repl-pop-to-buffer-on-connect t)

  ;; When there's a cider error, show its buffer and switch to it
  (setq cider-show-error-buffer t)
  (setq cider-auto-select-error-buffer t)

  ;; Where to store the cider history.
  (setq cider-repl-history-file "~/.emacs.d/cider-history")

  ;; Wrap when navigating history.
  (setq cider-repl-wrap-history t)

  :init
  ;; key bindings
  ;; these help me out with the way I usually develop web apps
  (defun cider-start-http-server ()
    (interactive)
    (cider-load-current-buffer)
    (let ((ns (cider-current-ns)))
      (cider-repl-set-ns ns)
      (cider-interactive-eval (format "(println '(def server (%s/start))) (println 'server)" ns))
      (cider-interactive-eval (format "(def server (%s/start)) (println server)" ns))))


  (defun cider-refresh ()
    (interactive)
    (cider-interactive-eval (format "(user/reset)")))

  (defun cider-user-ns ()
    (interactive)
    (cider-repl-set-ns "user"))

  :bind (:map clojure-mode-map
              ("C-c C-v" . 'cider-start-http-server)
              ("C-M-r" . 'cider-refresh)
              ("C-c u" . 'cider-user-ns)
              :map cider-mode-map
              ("C-c u" . 'cider-user-ns)))

;; (use-package flycheck-clojure
;;   :config (eval-after-load 'flycheck '(flycheck-clojure-setup)))

(provide 'init-clojure)
;;; init-clojure.el ends here
