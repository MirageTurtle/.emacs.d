;;; init-yaml.el -*- lexical-binding: t; -*-

(use-package yaml-mode
  :straight t
  :defer t)

(use-package yaml-pro
  :straight t
  :hook (yaml-mode . yaml-pro-mode))

(provide 'init-yaml)

;;; init-yaml.el ends here.
