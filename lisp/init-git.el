;;; init-git.el --- Git Configuration -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package transient
  :ensure t)

(use-package magit
  :ensure t
  :bind
  ("C-x g" . magit-status)
  ("C-c g" . magit-dispatch)
  ("C-c f" . magit-file-dispatch)
  :config
  (add-hook 'after-save-hook 'magit-after-save-refresh-status))

(provide 'init-git)

;;; init-git.el ends here
