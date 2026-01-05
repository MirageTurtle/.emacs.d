;;; init-env.el -*- lexical-binding: t; -*-

(use-package exec-path-from-shell
  :straight t
  :config
  (when (or *is-a-mac* *is-a-linux*)
    (exec-path-from-shell-initialize)))

(provide 'init-env)

;;; init-env.el ends here.
