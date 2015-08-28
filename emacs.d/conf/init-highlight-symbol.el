;;; init-highlight-symbol.el --- My configuration for this package
;;; commentary:
;;; code:
(require 'highlight-symbol)

(global-set-key [(super f3)] 'highlight-symbol)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
(global-set-key [(meta f3)] 'highlight-symbol-query-replace)

(provide 'init-highlight-symbol)
;;; init-highlight-symbol.el ends here
