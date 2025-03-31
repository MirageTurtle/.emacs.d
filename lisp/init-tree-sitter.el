;;; init-tree-sitter.el -*- lexical-binding: t; -*-
;;; Commentary:

;; This elisp file is for setting up tree-sitter,
;; instead of using the Emacs 29 treesit.

;;; Code:

(use-package tree-sitter
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
  :ensure t
  :hook
  (tree-sitter-after-on-hook . tree-sitter-hl-mode)
  (emacs-lisp-mode . tree-sitter-hl-mode)
  (python-mode . tree-sitter-hl-mode)
  :config
  (global-tree-sitter-mode)
  (setq tree-sitter-load-paths "~/.emacs.d/tree-sitter/")
  ;; for major mode language register
  (setq tree-sitter-major-mode-language-alist
	'((emacs-lisp-mode . elisp)
	  (python-ts-mode . python)
	  (bash-ts-mode . bash)
	  (rust-ts-mode . rust)
	  (go-ts-mode . go)
	  (lisp-data-mode . elisp))))

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter)

(use-package ts-fold
  :straight (ts-fold :type git :host github :repo "MirageTurtle/ts-fold")
  :hook
  (prog-mode . ts-fold-mode)
  (tree-sitter-after-on . ts-fold-mode)
  :bind
  ("s-<return>" . ts-fold-toggle))

(provide 'init-tree-sitter)
;;; init-tree-sitter.el ends here
