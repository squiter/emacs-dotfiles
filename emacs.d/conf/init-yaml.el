(use-package yaml-mode
  :mode (("\\.yml$" . yaml-mode)
         ("\\.yaml$" . yaml-mode)
         ("\\.yml\\.example$" . yaml-mode))

  :hook (yaml-mode . linum-mode))

  (provide 'init-yaml)
