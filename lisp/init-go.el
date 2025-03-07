;;; init-go.el --- Go configuration -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(require 'init-prog)

(use-package go-mode
  :ensure t
  :hook
  (go-mode . lsp-deferred)
  (go-mode . go-format-on-save-mode)
  :config
  (reformatter-define go-format
    :program "gofmt"
    :args '("-s"))
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode)))

(provide 'init-go)

;;; init-go.el ends here.
