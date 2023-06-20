;;; init-org-mobile.org --- My custom configs for use MobileOrg
;;; commentary:
;;; code:


(add-hook 'after-init-hook 'org-mobile-pull)
(add-hook 'kill-emacs-hook 'org-mobile-push)

;; Push when saving org files
(add-hook
  'after-save-hook
  (lambda ()
     (if (string-match "org$" buffer-file-name)
        (org-mobile-push)
     )
  ))

;; Set to the location of your Org files on your local system
(setq org-directory *user-org-cache-directory*)

;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull (path-join *user-org-cache-directory* "mobile-refile.org"))

;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory (path-join *user-dropbox-directory* "Apps/MobileOrg"))

;; moble sync
(defvar org-mobile-sync-timer nil)
(defvar org-mobile-sync-idle-secs (* 60 10))
(defun org-mobile-sync ()
  "Seting sync to pull and push."
  (interactive)
  (org-mobile-pull)
  (org-mobile-push))
(defun org-mobile-sync-enable ()
  "Enable mobile org idle sync."
  (interactive)
  (message "org-mobile-sync enabled!")
  (setq org-mobile-sync-timer
        (run-with-idle-timer org-mobile-sync-idle-secs t
                             'org-mobile-sync)));
(defun org-mobile-sync-disable ()
  "Disable mobile org idle sync."
  (interactive)
  (message "org-mobile-sync disabled!")

  (cancel-timer org-mobile-sync-timer))
(org-mobile-sync-enable)

(provide 'init-org-mobile)
;;; init-org-mobile.el ends here
