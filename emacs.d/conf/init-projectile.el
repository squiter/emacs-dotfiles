(require 'projectile)

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
(defvar default-project-source
  "~/projetos/")

(defvar project-sources
  (list
   default-project-source
   "~/projetos/locaweb/"))

;; helm integration for opening projects

(defun helm-rr/open-project ()
  "Bring up a Project search interface in helm."
  (interactive)
  (helm :sources '(helm-source-list-projects)
	:buffer "*helm-list-projects*"))

(defvar helm-source-list-projects
  '((name . "Open Project")
    (volatile)
    (delayed)
    (candidates . rr/list-projects)
    (action-transformer . rr/open-project)))

(defun rr/list-projects ()
  "Lists all projects given project sources."
  (cl-labels ((dir-to-files (dir)
			    (if (file-exists-p dir)
				(directory-files dir t directory-files-no-dot-files-regexp)))
	      (flatten (x)
		       (cond ((null x) nil)
			     ((listp x) (append (car x) (flatten (cdr x)))))))
    (progn (flatten (mapcar #'dir-to-files  project-sources)))))

(defun rr/open-project (actions path)
  "Do nothing with ACTIONS. Open project given PATH."
  ;; TODO: Add default file get.
  (cl-flet ((find-default-file () (if (file-exists-p (expand-file-name "Gemfile" path))
				      (expand-file-name "Gemfile" path)
				    path)))
    (find-file (find-default-file))))

(global-set-key (kbd "C-c o") 'helm-rr/open-project)


(provide 'init-projectile)
