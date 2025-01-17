;; init-env.el -*- lexical-binding: t; -*-

(require 'exec-path-from-shell) ;; if not using the ELPA package
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(provide 'init-env)

;; init-env.el ends here.
