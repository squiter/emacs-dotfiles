;;; init-telephone.el --- My telephone package configurations
;;; Commentary:
;;; Code:

(add-to-list 'load-path (expand-file-name "vendor/git-emacs" user-emacs-directory))
(autoload 'git--update-all-state-marks "git-modeline" nil t)
(add-hook 'find-file-hook 'git-status-in-modeline t)
(add-hook 'magit-revert-buffer-hook 'git-status-in-modeline t)

(defun git-status-in-modeline ()
  (if (and vc-mode (string-match "^ Git" (substring-no-properties vc-mode)))
      (git--update-all-state-marks)))

(defun change-git-status-position (&rest _)
  (let ((git-status (car mode-line-format)))
    (setq mode-line-format (-insert-at 3 git-status (cdr mode-line-format)))))

(advice-add 'git--install-state-mark-modeline
            :after
            #'change-git-status-position)

(require 'telephone-line)

(set-face-attribute 'telephone-line-accent-active nil
                    :background "#7C71C4"
                    :foreground "black"
                    :box nil)

(set-face-attribute 'mode-line-inactive nil
                    :box nil
                    :foreground "#7C71C4"
                    :background "#20272e")

(set-face-attribute 'mode-line nil
                    :box '(:line-width 1 :style raised)
                    :foreground "#7C71C4"
                    :background "#20272e")

(telephone-line-defsegment* squiter/telephone-line-buffer-segment ()
  `("",(telephone-line-raw mode-line-buffer-identification t)))

(telephone-line-defsegment* squiter/telephone-line-projectile-project-name ()
  `("📁 " ,(projectile-project-name)))

(telephone-line-defsegment* squiter/major-mode ()
  (propertize (all-the-icons-icon-for-mode major-mode)
              'face `(:height 0.9 :family ,(all-the-icons-fileicon-family))
              'display '(raise -0.1)
              'help-echo major-mode))

(telephone-line-defsegment* squiter/file-icon ()
  (propertize (all-the-icons-icon-for-file (buffer-name))
              'face `(:height 0.9 :family ,(all-the-icons-fileicon-family))
              'display '(raise -0.1)))


(defun shackra/vc-state ()
  (vc-state (buffer-file-name (current-buffer))))

(telephone-line-defsegment* shackra-flycheck-status ()
  (let* ((text (pcase flycheck-last-status-change
                 (`finished (if flycheck-current-errors
                                (let ((count (let-alist (flycheck-count-errors flycheck-current-errors)
                                               (+ (or .warning 0) (or .error 0)))))
                                  (format " ✖ %s Issue(s)" count))
                              " ✔ No Issues"))
                 (`running     " ⟲ Working...")
                 (`no-checker  " ⚠ No Checker")
                 (`not-checked " ✖ Disabled")
                 (`errored     " ⚠ Error")
                 (`interrupted " ⛔ Interrupted")
                 (`suspicious  ""))))
    (propertize text
                'help-echo "Show Flycheck Errors"
                'mouse-face '(:box 1)
                'face `(:height 0.8) 'display '(raise 0.1)
                'local-map (make-mode-line-mouse-map
                            'mouse-1 (lambda () (interactive) (flycheck-list-errors))))))

(telephone-line-defsegment* shackra-buffer-vc-modified-segment ()
  (cond ((buffer-modified-p)
         (propertize
          (format " %s" (all-the-icons-faicon "pencil"))
          'face `(:foreground "orange" :height 0.8 :family ,(all-the-icons-faicon-family))
          'display '(raise 0.1) 'help-echo "Modified buffer."))

        ((eq (shackra/vc-state) 'edited)
         (propertize
          (format " %s" (all-the-icons-faicon "pencil"))
          'face `(:height 0.8 :family ,(all-the-icons-faicon-family))
          'display '(raise 0.1) 'help-echo "Modified buffer, changes not commited."))

        ((eq (shackra/vc-state) 'unregistered)
         (propertize
          (format " %s" (all-the-icons-faicon "question"))
          'face `(:height 0.8 :family ,(all-the-icons-faicon-family))
          'display '(raise 0.1) 'help-echo "Archivo sin registrar al VCS."))
        ((eq (shackra/vc-state) 'missing)
         (propertize
          (format " %s" (all-the-icons-faicon "exclamation"))
          'face `(:height 0.8 :family ,(all-the-icons-faicon-family))
          'display '(raise 0.1) 'help-echo "File only exists in VCS, not in HD."))

        ((eq (shackra/vc-state) 'ignored)
         (propertize
          (format " %s" (all-the-icons-faicon "ban"))
          'face `(:height 0.8 :family ,(all-the-icons-faicon-family))
          'display '(raise 0.1) 'help-echo "Ignored file."))

        ((eq (shackra/vc-state) 'added)
         (propertize
          (format " %s" (all-the-icons-faicon "plus"))
          'face `(:height 0.8 :family ,(all-the-icons-faicon-family))
          'display '(raise 0.1) 'help-echo "Added file."))))

(setq telephone-line-lhs
      '((accent . (squiter/telephone-line-buffer-segment
                   telephone-line-process-segment
                   telephone-line-erc-modified-channels-segment))
        (nil . (telephone-line-process-segment
                shackra-buffer-vc-modified-segment
                squiter/telephone-line-projectile-project-name
                shackra-flycheck-status
                ))))

(setq telephone-line-rhs
      '((nil    . (squiter/major-mode
                   ;; squiter/file-icon
                   telephone-line-misc-info-segment))
        (accent . (telephone-line-position-segment))))

(setq telephone-line-primary-left-separator 'telephone-line-identity-left
      telephone-line-left-separator 'telephone-line-identity-left
      telephone-line-secondary-left-separator 'telephone-line-identity-hollow-left)

(setq telephone-line-primary-right-separator 'telephone-line-identity-right
      telephone-line-right-separator 'telephone-line-identity-right
      telephone-line-secondary-right-separator 'telephone-line-identity-hollow-right)

(telephone-line-mode 1)

(provide 'init-telephone)
;;; init-telephone.el ends here
