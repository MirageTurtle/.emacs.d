;;; init-beancount.el -*- lexical-binding: t; -*-

(use-package beancount
  :straight t
  :defer t
  :mode ("\\.beancount\\'" . beancount-mode))

(provide 'init-beancount)
;;; init-beancount.el ends here
