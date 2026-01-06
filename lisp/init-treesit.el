;; init-treesit.el -*- lexical-binding: t; -*-
;; This file is for configuring treesit built-in to Emacs.

(require 'init-prog)
(require 'treesit)

(use-package treesit-fold
  :straight (treesit-fold :type git :host github :repo "emacs-tree-sitter/treesit-fold")
  :after treesit
  :init
  (setq treesit-language-source-alist
        '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
          (c          . ("https://github.com/tree-sitter/tree-sitter-c"))
          (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
          (elisp      . ("https://github.com/Wilfred/tree-sitter-elisp"))
          (go         . ("https://github.com/tree-sitter/tree-sitter-go"))
          (gomod      . ("https://github.com/camdencheek/tree-sitter-go-mod"))
          (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
          (python     . ("https://github.com/tree-sitter/tree-sitter-python"))
          (rust       . ("https://github.com/tree-sitter/tree-sitter-rust"))
          (toml       . ("https://github.com/tree-sitter/tree-sitter-toml"))
          (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript"))
	  (typst      . ("https://github.com/uben0/tree-sitter-typst"))))
  :hook
  (prog-mode . treesit-fold-mode)
  :bind
  ("s-<return>" . treesit-fold-toggle))

(provide 'init-treesit)

;; init-treesit.el ends here
