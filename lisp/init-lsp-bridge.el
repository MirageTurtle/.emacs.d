;;; init-lsp-bridge.el --- lsp-bridge configuration -*- lexical-binding: t -*-
;;; Commentary:
;;;  lsp-bridge depends on markdown-mode and yasnippet, the latter of which is installed in `init-efficiency.el'
;;;  todo: lsp-bridge-popup-documentation-scroll-up/down doesn't work well
;;;  todo: lsp-bridge doesn't work well with remote host

;;; Code:

(use-package markdown-mode
  :straight t)

(use-package lsp-bridge
  :straight '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
                         :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
                         :build (:not compile))
  :init
  (global-lsp-bridge-mode)
  :hook
  ((prog-mode . lsp-bridge-mode))
  :bind
  (:map lsp-bridge-mode
	("M-s-j" . lsp-bridge-diagnostic-jump-next)
	("M-s-k" . lsp-bridge-diagnostic-jump-prev)
	("M-s-n" . lsp-bridge-popup-documentation-scroll-up)
	("M-s-p" . lsp-bridge-popup-documentation-scroll-down)
        )
  :config
  (setq lsp-bridge-python-lsp-server 'ty)
  (setq lsp-bridge-python-multi-lsp-server 'ty_ruff)
  (setq lsp-bridge-python-command (expand-file-name "~/Documents/venv/emacs-python/bin/python3"))
  (setq lsp-bridge-default-mode-hooks '(copilot-mode))
  (setq lsp-bridge-popup-documentation t)
  (setq lsp-bridge-enable-hover-diagnostic t)
  (setq lsp-bridge-enable-document-highlight t)
  (setq lsp-bridge-enable-diagnostics t)
  (setq lsp-bridge-enable-inlay-hint t)
  (setq lsp-bridge-symbols-enable-which-func t)
  )

;; Some Issue
;; https://github.com/manateelazycat/lsp-bridge?tab=readme-ov-file#customize-language-server-configuration-file
;; (defun enable-lsp-bridge()
;;   (when-let* ((project (project-current))
;;               (project-root (nth 2 project)))
;;     (setq-local lsp-bridge-user-langserver-dir project-root
;;                 lsp-bridge-user-multiserver-dir project-root))
;;   (lsp-bridge-mode))


(provide 'init-lsp-bridge)

;;; init-lsp-bridge.el ends here.
