;; Set to the location of your Org files on your local system
(setq org-directory *user-org-cache-directory*)
;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull (path-join *user-org-cache-directory* "mobile-refile.org"))
;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory (path-join *user-dropbox-directory* "Apps/MobileOrg"))

(provide 'init-org-mobile)
