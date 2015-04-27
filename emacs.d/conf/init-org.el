(require 'org)

(defconst *user-org-cache-directory*
  (path-join *user-dropbox-directory* "org")
  "Path to user's org cache store.")

;; Tags with fast selection keys
(setq org-tag-alist (quote ((:startgroup) ;; location group
                            ("@office" . ?O)
                            ("@home" . ?H)
                            ("PERSONAL" . ?P)
                            ("WORK" . ?W)
                            (:endgroup)
                            (:startgroup) ;; note type
                            ("ERROR" . ?e)
                            ("FACT" . ?f)
                            ("IDEA" . ?i)
                            ("QUESTION" . ?q)
                            (:endgroup)
                            ("WAITING" . ?w)
                            ("HOLD" . ?h)
                            ("NOTE" . ?n)
                            ("CANCELLED" . ?c)
                            ("FLAGGED" . ??))))

(defun bh/hide-other ()
  (interactive)
  (save-excursion
    (org-back-to-heading 'invisible-ok)
    (hide-other)
    (org-cycle)
    (org-cycle)
    (org-cycle)))

(defun bh/set-truncate-lines ()
  "Toggle value of truncate-lines and refresh window display."
  (interactive)
  (setq truncate-lines (not truncate-lines))
  ;; now refresh window display (an idiom from simple.el):
  (save-excursion
    (set-window-start (selected-window)
                      (window-start (selected-window)))))

(defun bh/make-org-scratch ()
  (interactive)
  (find-file "/tmp/publish/scratch.org")
  (gnus-make-directory "/tmp/publish"))

(defun bh/switch-to-scratch ()
  (interactive)
  (switch-to-buffer "*scratch*"))


(require 'init-org-keybinds)
(require 'init-org-agenda)
(require 'init-org-captures)
(require 'init-org-speed-commands)
(require 'init-org-refile)
(require 'init-org-agenda-view)
(require 'init-org-agenda-tunned)
(require 'init-org-clock)
(require 'init-org-mobile)
(require 'init-org-projects)
(require 'init-org-habit)

(provide 'init-org)
