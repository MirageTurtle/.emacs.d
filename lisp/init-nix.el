;;; init-nix.el -*- lexical-binding: t; -*-

;; Code:
(require 'init-prog)

(use-package nix-mode
  :ensure t
  :defer t
  :mode "\\.nix\\'")

(provide 'init-nix)
