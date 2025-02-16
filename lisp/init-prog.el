;;; init-prog.el -*- lexical-binding: t; -*-
;;; Commentary:

;;; Log:

;; 2025-02-16:
;;   remove ts-fold, cause I try to use hs-minor-mode instead

;; Programming related configurations

;;; Code:

(add-hook 'prog-mode-hook #'show-paren-mode) ; highlight electric pair in program mode
(add-hook 'prog-mode-hook #'hs-minor-mode) ; fold code block in program mode

(use-package reformatter
  :ensure t)


;; (use-package ts-fold
;;   :ensure t
;;   :straight (ts-fold :type git :host github :repo "emacs-tree-sitter/ts-fold")
;;   :hook (prog-mode . ts-fold-mode))

;; debug
(use-package dape
  ;; require jsonrpc >= 1.0.24, I use 1.0.25
  :ensure t
  :config
  (setq dape-buffer-window-arrangement 'right))

(provide 'init-prog)

;;; init-prog.el ends here
