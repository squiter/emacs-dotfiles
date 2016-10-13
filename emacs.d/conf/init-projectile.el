;;; init-projectile.el --- My Projectile configrations
;;; Commentary:
;;; Code:

(require 'projectile)

(counsel-projectile-on)

;; projectile-rails
(add-hook 'projectile-mode-hook 'projectile-rails-on)

(global-set-key (kbd "C-x f") 'counsel-projectile-find-file)
(global-set-key (kbd "C-c p s a") 'counsel-projectile-ag)
(global-set-key (kbd "C-x B") 'counsel-projectile-switch-to-buffer)

(projectile-global-mode)

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
    (find-file (or elected path))))

(global-set-key (kbd "C-c o") 'squiter/ivy-open-project)

(provide 'init-projectile)
;;; init-projectile.el ends here
