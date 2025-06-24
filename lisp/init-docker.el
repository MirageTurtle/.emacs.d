;;; init-docker.el -*- lexical-binding: t; -*-

;;; Code:
(use-package dockerfile-mode
  :ensure t
  :defer t
  :mode ("Dockerfile\\'" . dockerfile-ts-mode))


(provide 'init-docker)
;;; init-docker.el ends here
