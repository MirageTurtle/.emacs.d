;;; init-lsp-bridge.el --- lsp-bridge configuration -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;; dependencies
(require 'yasnippet)
(require 'markdown-mode)

(use-package lsp-bridge
  :straight '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
            :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
            :build (:not compile))
  :ensure t
  ;; :init
  ;; (global-lsp-bridge-mode)
  ;; :hook
  ;; ((prog-mode . lsp-bridge-mode)
  ;;  ((latex-mode LaTeX-mode) . lsp-bridge-mode)
  ;;  (markdown-mode . lsp-bridge-mode))
  :bind (:map lsp-bridge-mode
	;; ("M-s-j" . lsp-bridge-diagnostic-jump-next)
	;; ("M-s-k" . lsp-bridge-diagnostic-jump-prev)
	("M-s-n" . lsp-bridge-popup-documentation-scroll-up)
	("M-s-p" . lsp-bridge-popup-documentation-scroll-down))
  :config
  (setq lsp-bridge-python-lsp-server 'basedpyright)
  (setq lsp-bridge-python-multi-lsp-server 'basedpyright_ruff)
  (setq lsp-bridge-tex-lsp-server 'texlab)
  ;; (setq lsp-bridge-enable-log t)
  (setq lsp-bridge-enable-log nil)
  ;; (setq lsp-bridge-enable-debug t)
  (setq lsp-bridge-python-command (expand-file-name "~/Documents/venv/lsp-bridge/bin/python3"))
  ;; (setq acm-enable-copilot t)
  (setq acm-enable-codeium nil)
  (setq lsp-bridge-default-mode-hooks '(copilot-mode))
  (setq lsp-bridge-popup-documentation t)
  (setq lsp-bridge-enable-diagnostics t))

;; Some Issue
;; https://github.com/manateelazycat/lsp-bridge?tab=readme-ov-file#customize-language-server-configuration-file
(defun enable-lsp-bridge()
  (when-let* ((project (project-current))
              (project-root (nth 2 project)))
    (setq-local lsp-bridge-user-langserver-dir project-root
                lsp-bridge-user-multiserver-dir project-root))
  (lsp-bridge-mode))


(provide 'init-lsp-bridge)

;;; init-lsp-bridge.el ends here.
