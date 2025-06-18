;;; init-typst.el --- Typst configuration -*- lexical-binding: t; -*-
;; Commentary:
;;
;; Log:
;; 2025-06-18:
;;   * add `typst-ts-mode' for Typst support

;; Code:
(use-package typst-ts-mode
  :ensure t
  :mode ("\\.typ\\'" . typst-ts-mode)
  :hook
  (typst-ts-mode . show-paren-mode) ; highlight electric pair in Typst mode
  (typst-ts-mode . hs-minor-mode) ; fold code block in Typst mode
  :config
  (setq typst-ts-markup-header-same-height t)
  (setq typst-ts-markup-header-scale '(2.0 1.7 1.4 1.1 1.0 1.0)))

(provide 'init-typst)

;;; init-typst.el ends here
