;;; init-env.el -*- lexical-binding: t; -*-

;; if exec-path-from-shell is not installed, install it
(unless (package-installed-p 'exec-path-from-shell)
  (package-refresh-contents)
  (package-install 'exec-path-from-shell))
(require 'exec-path-from-shell)
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (or *is-a-mac* *is-a-linux*)
    (exec-path-from-shell-initialize)))

(provide 'init-env)

;;; init-env.el ends here.
