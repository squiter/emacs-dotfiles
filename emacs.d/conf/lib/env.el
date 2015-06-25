;;; env.el --- support functions for working with environment variables
;;; Commentary:
;;; Code:

(defun getenv-or (env value)
  "Fetch the value of ENV or, if it is not set, return VALUE."
  (if (getenv env)
      (getenv env)
    value))

(setenv "LC_ALL" "en_US.utf-8")
(setenv "LANG" "en_US.utf-8")

(provide 'lib/env)
;;; env.el ends here
