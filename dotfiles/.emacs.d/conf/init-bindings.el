;; TODO: Maybe I should delete this file
"Stolen from milhouse, used to expose bindings for term"
;; Keybindings macros
(defvar bindings-to-expose
  '("M-x"
    "C-c C-f"
    "C-M-f"
    "C-M-b"
    "C-h"
    "C-c o"
    "M-h"
    "M-k"
    "M-o"
    "M-1"
    "M-2"
    "M-3"
    "M-0"
    "M-i"
    "M-l")
  "Custom keybindings to expose on every mode.")

(defun expose-rr/default-bindings (mode-map)
  (expose-bindings mode-map bindings-to-expose))

(defmacro expose-global-keybinding (binding map)
  `(define-key ,map ,binding `,(lookup-key `,(current-global-map) ,binding)))

(defmacro expose-bindings (map bindings)
  `(dolist (bnd ,bindings)
     `,(expose-global-keybinding `,(kbd bnd) ,map)))

(provide 'init-bindings)
