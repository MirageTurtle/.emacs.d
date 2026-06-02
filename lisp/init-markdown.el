;;; init-markdown.el --- Markdown configuration -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package markdown-ts-mode
  :straight t
  :config
  (add-to-list 'treesit-language-source-alist '(markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown/src"))
  (add-to-list 'treesit-language-source-alist '(markdown-inline "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown-inline/src")))

(use-package grip-mode
  :straight t
  :config (setq grip-command 'go-grip)
  :hook ((markdown-mode org-mode) . grip-mode)
  :hook ((markdown-ts-mode org-mode) . grip-mode))

(provide 'init-markdown)

;;; init-markdown.el ends here
