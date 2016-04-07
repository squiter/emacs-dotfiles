(require 'org)

(add-to-list 'load-path (expand-file-name "conf/orgmode" user-emacs-directory))

(defconst *user-org-cache-directory*
  (path-join *user-dropbox-directory* "org")
  "Path to user's org cache store.")

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
(require 'init-org-crypt)
(require 'init-org-id)
(require 'init-org-notifications)
(require 'init-org-babel)
(require 'init-org-alfred)
(require 'init-org-bullets)
(require 'org-drill)

(setq org-ellipsis "⤵")

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
			    ("crypt" . ?k)
			    ("BORROWED" . ?b)
			    ("BILLS" . ?B)
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

;; Org Crypt configuration
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))

(setq org-crypt-key nil)
;; GPG key to use for encryption
;; Either the Key ID or set to nil to use symmetric encryption.

(setq auto-save-default nil)
;; Auto-saving does not cooperate with org-crypt.el: so you need
;; to turn it off if you plan to use org-crypt.el quite often.
;; Otherwise, you'll get an (annoying) message each time you
;; start Org.

;; To turn it off only locally, you can insert this:
;;
;; # -*- buffer-auto-save-file-name: nil; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; Org mode 8.3    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-repair-property-drawers ()
  "Fix properties drawers in current buffer.
 Ignore non Org buffers."
  (interactive)
  (when (eq major-mode 'org-mode)
    (org-with-wide-buffer
     (goto-char (point-min))
     (let ((case-fold-search t)
           (inline-re (and (featurep 'org-inlinetask)
                           (concat (org-inlinetask-outline-regexp)
                                   "END[ \t]*$"))))
       (org-map-entries
        (lambda ()
          (unless (and inline-re (org-looking-at-p inline-re))
            (save-excursion
              (let ((end (save-excursion (outline-next-heading) (point))))
                (forward-line)
                (when (org-looking-at-p org-planning-line-re) (forward-line))
                (when (and (< (point) end)
                           (not (org-looking-at-p org-property-drawer-re))
                           (save-excursion
                             (and (re-search-forward org-property-drawer-re end t)
                                  (eq (org-element-type
                                       (save-match-data (org-element-at-point)))
                                      'drawer))))
                  (insert (delete-and-extract-region
                           (match-beginning 0)
                           (min (1+ (match-end 0)) end)))
                  (unless (bolp) (insert "\n"))))))))))))

(provide 'init-org)
