;; init-treesit.el -*- lexical-binding: t; -*-
;; This file is for configuring treesit built-in to Emacs.

(use-package treesit
  :straight nil)


(use-package treesit-fold
  :straight (treesit-fold :type git :host github :repo "emacs-tree-sitter/treesit-fold")
  :after treesit
  :init
  (setq treesit-language-source-alist
        '((elisp      . ("https://github.com/Wilfred/tree-sitter-elisp"))
          (rust       . ("https://github.com/tree-sitter/tree-sitter-rust"))
          (toml       . ("https://github.com/tree-sitter/tree-sitter-toml"))
          (go         . ("https://github.com/tree-sitter/tree-sitter-go"))
          (gomod      . ("https://github.com/camdencheek/tree-sitter-go-mod"))
          (python     . ("https://github.com/tree-sitter/tree-sitter-python"))
          (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
          (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript"))))
  :hook
  (prog-mode . treesit-fold-mode)
  :bind
  ("s-<return>" . treesit-fold-toggle))

(provide 'init-treesit)

;; init-treesit.el ends here
