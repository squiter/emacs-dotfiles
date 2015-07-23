(require 'projectile)

;; projectile-rails
(add-hook 'projectile-mode-hook 'projectile-rails-on)

(global-set-key (kbd "C-x f") 'helm-projectile)
(global-set-key (kbd "C-c p s a") 'helm-projectile-ack)

(projectile-global-mode)

(setq projectile-completion-system 'helm)
(setq projectile-switch-project-action 'helm-projectile)
(setq helm-projectile-sources-list '(helm-source-projectile-buffers-list
				     helm-source-projectile-files-list))

;; Projectile enable caching
(setq projectile-enable-caching t)

;;
;;; Jumping between projects (stolen from milhouse)
;;

;; variables
(defvar default-project-source (path-join *user-home-directory* "projetos/"))

(defvar project-sources
  (list
   default-project-source
   (path-join *user-home-directory* "projetos/locaweb/")))

;; helm integration for opening projects
;; Jumping between projects
;;
(defvar rr/project-sources
  '("~/projetos/"
    "~/projetos/locaweb/"))

(defvar rr/default-file-regexps
  '("Gemfile$"
    "mix.exs$"
    "Readme"
    "README"))

(defun rr/helm-open-project ()
  "Bring up a Project search interface in helm."
  (interactive)
  (helm :sources '(rr/helm-open-project--source)
	:buffer "*helm-list-projects*"))

(defvar rr/helm-open-project--source
  '((name . "Open Project")
    (delayed)
    (candidates . rr/list-projects)
    (action . rr/open-project)))

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

(global-set-key (kbd "C-c o") 'rr/helm-open-project)

(provide 'init-projectile)
