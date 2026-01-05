;;; init-nix.el -*- lexical-binding: t; -*-

;; Code:
(require 'init-prog)

(use-package nix-mode
  :straight t
  :defer t
  :mode "\\.nix\\'")

(provide 'init-nix)
