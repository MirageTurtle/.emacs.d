;;; init-yaml.el -*- lexical-binding: t; -*-

(use-package yaml-mode
  :straight t)

(use-package yaml-pro
  :straight t
  ;; :after (yaml-mode)
  :config
  (add-hook 'yaml-mode-hook #'yaml-pro-mode))

(provide 'init-yaml)

;;; init-yaml.el ends here.
