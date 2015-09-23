;;; init-org-alfred.el --- Settings to make alfred capture org tasks
;;; This is a fuction to help get a frame created for alfred-org-capture

;;; Commentary:
;;;
;;;  This is only an mvp, taken from: http://comments.gmane.org/gmane.emacs.orgmode/76348

;;; Code:
(defun make-orgcapture-frame ()
  "Create a new frame and run `org-capture'."
  (interactive)
  (make-frame '((name . "remember") (width . 80) (height . 16)
                (top . 400) (left . 300)
                (font . "-apple-Monaco-medium-normal-normal-*-13-*-*-*-m-0-iso10646-1")
                ))
  (select-frame-by-name "remember")
  (org-capture))

(provide 'init-org-alfred)
;;; init-org-alfred.el ends here
