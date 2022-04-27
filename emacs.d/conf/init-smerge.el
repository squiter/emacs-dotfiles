;;; init-smerge.el --- My configurations to git merge
;;; commentary:
;;; code:

(use-package smerge-mode
  :bind (("C-c s RET" . smerge-keep-current)
         ("C-c s n" . smerge-next)
         ("C-c s p" . smerge-prev)
         ("C-c s a" . smerge-keep-all)
         ("C-c s b" . smerge-keep-base)
         ("C-c s m" . smerge-keep-upper)
         ("C-c s u" . smerge-keep-upper)
         ("C-c s l" . smerge-keep-lower))
  :init
  (progn
    (defun sm-try-smerge ()
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^<<<<<<< " nil t)
          (smerge-mode 1))))
    (add-hook 'find-file-hook 'sm-try-smerge)))

(provide 'init-smerge)
;;; init-smerge.el ends here
