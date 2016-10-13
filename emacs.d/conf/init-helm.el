;;; init-helm.el --- My Helm configurations
;;; Commentary:
;;; Code:

(require 'helm-config)

(helm-mode t)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "<backtab>") 'helm-select-action)

(setq helm-quick-update t
      helm-buffers-fuzzy-matching t
      helm-buffer-max-length 25
      helm-buffer-details-flag nil
      helm-display-header-line nil)

(defun bsl/filter-buffers (buffer-list)
  (delq nil (mapcar
             (lambda (buffer)
               (cond
                ((eq (with-current-buffer buffer major-mode)  'dired-mode) nil)
                ((eq (with-current-buffer buffer major-mode)  'org-mode) nil)
                ((eq (with-current-buffer buffer major-mode)  'org-agenda-mode) nil)
                (t buffer)))
             buffer-list)))

(advice-add 'helm-skip-boring-buffers :filter-return 'bsl/filter-buffers)

(defun squiter/hotspots ()
  "Helm interface to my hotspots/bookmarks"
  (interactive)
  (helm :sources `(((name . "Mail and News")
                    (candidates . (("Calendar" . (lambda ()  (browse-url "https://www.google.com/calendar/render")))
                                   ("RSS" . (lambda () (browse-url "https://feedly.com")))
                                   ("Agenda" . (lambda () (org-agenda "" " ")))))
                    (action . (("Open" . (lambda (x) (funcall x))))))
                   ((name . "My Locations")
                    (candidates . ((".emacs.d" . "~/.emacs.d/" )
                                   ("Downloads" . "~/Downloads/" )))
                    (action . (("Open" . (lambda (x) (find-file x))))))
                   ((name . "Work")
                    (candidates . (("Pontos" . (lambda () (browse-url "https://portalrh.cservices.com.br/PortalLocaweb/")))))
                    (action . (("Open" . (lambda () (funcall x))))))
                   helm-source-recentf
                   helm-source-bookmarks
                   helm-source-bookmark-set)))

(provide 'init-helm)
;;; init-helm.el ends here
