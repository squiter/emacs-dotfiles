;;; init-projectile.el --- Projects configurations
;;
;; Copyright (C) 2018 Brunno dos Santos <emacs at brunno dot me>
;;
;; Author: Brunno dos Santos @squiter
;; URL: http://github.com/squiter/emacs-dotfiles
;;
;; Created: 10 August 2018
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
(use-package projectile
  :bind-keymap ("C-c p" . projectile-command-map)
  :bind
  (("C-c o" . squiter/ivy-open-project)
   ("C-x f" . projectile-find-file)
   :map projectile-command-map
   ("s a" . projectile-ag))

  :commands (projectile-project-p)
  :config
  (projectile-mode)

  ;;
  ;;; Jumping between projects (stolen from milhouse)
  ;;

  ;; variables
  (defvar default-project-source *projects-directory*)

  (defvar project-sources *all-project-directories*)

  ;; integration for opening projects
  ;; Jumping between projects
  ;;
  (defvar rr/project-sources *all-project-directories*)

  (defvar rr/default-file-regexps
    '("Gemfile$"
      "mix.exs$"
      "Readme"
      "README"))

  (defun squiter/ivy-open-project ()
    "Bring up a Project search interface in ivy."
    (interactive)
    (ivy-read "Open Project: "
              (rr/list-projects)
              :sort t
              :action 'rr/open-project))

  (defun rr/list-projects ()
    "Lists all projects given project sources."
    (->> rr/project-sources
         (-filter 'file-exists-p)
         (-mapcat (lambda (dir) (directory-files dir t directory-files-no-dot-files-regexp)))))

  (defun rr/open-project (path)
    "Open project available at PATH."
    ;; TODO: Add default file get.
    (let* ((candidates (-mapcat (lambda (d) (directory-files path t d)) rr/default-file-regexps))
           (elected (car candidates)))
      (find-file (or elected path)))))

(use-package projectile-rails
  :after projectile
  :hook (projectile-mode . projectile-rails-on))

;; TODO: waiting https://github.com/ericdanan/counsel-projectile/pull/92
;; (use-package counsel-projectile
;;   :after (projectile counsel)
;;   :bind
;;   ("C-x f" . counsel-projectile-find-file)
;;   ("C-c p s a" . counsel-projectile-ag)
;;   ("C-x C-b" . counsel-projectile-switch-to-buffer)

;;   :config (counsel-projectile-mode))

(provide 'init-projectile)
;;; init-projectile.el ends here
