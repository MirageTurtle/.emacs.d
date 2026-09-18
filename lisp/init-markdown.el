;;; init-markdown.el --- Markdown configuration -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package markdown-ts-mode
  :straight (:type built-in)
  :mode (("\\.md\\'"       . markdown-ts-mode)
         ("\\.markdown\\'" . markdown-ts-mode))
  :config
  (require 'markdown-ts-mode-x))

;; grip-mode for preview markdown files using go-grip
(use-package grip-mode
  :straight t
  :config (setq grip-command 'go-grip)
  :bind (:map markdown-mode-command-map
              ("g" . grip-mode)))

(provide 'init-markdown)

;;; init-markdown.el ends here
