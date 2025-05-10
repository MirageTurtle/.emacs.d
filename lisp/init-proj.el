;;; init-proj.el -*- lexical-binding: t; -*-

;;; Code:

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  (setq projectile-switch-project-action #'projectile-dired)
  (setq projectile-enable-caching t)
  (setq projectile-indexing-method 'native)
  (setq projectile-completion-system 'ivy)
  :config
  (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map) ; keybinding for projectile commands to override default C-x p
  )

(provide 'init-proj)
