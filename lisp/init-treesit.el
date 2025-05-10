;; init-treesit.el -*- lexical-binding: t; -*-
;; This file is for configuring treesit built-in to Emacs.

(use-package treesit
  :straight nil)


(use-package treesit-fold
  :straight (treesit-fold :type git :host github :repo "emacs-tree-sitter/treesit-fold")
  :after treesit
  :hook
  (prog-mode . treesit-fold-mode)
  :bind
  ("s-<return>" . treesit-fold-toggle))

(provide 'init-treesit)

;; init-treesit.el ends here
