;;; init-c.el -*- lexical-binding: t; -*-

;;; Code:
(use-package c-mode
  :ensure nil
  :defer t
  :config
  (setq-default c-basic-offset 4
		tab-width 4
		indent-tabs-mode nil))

(use-package clang-format
  :straight t
  :defer t
  :config
  (setq clang-format-style "file"))

(provide 'init-c)
