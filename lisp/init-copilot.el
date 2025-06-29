;;; init-copilot.el --- Copilot configuration -*- lexical-binding: t -*-
;;; Commentary:

;; editorconfig and jsonrpc are required by copilot

;;; Code:

(use-package editorconfig
  :ensure t)

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("dist" "*.el"))
  :ensure t
  :hook (prog-mode . copilot-mode)
  :config
  (setq copilot-indent-offset-warning-disable t)
  ;; (setq copilot-network-proxy '(:host "127.0.0.1" :port 7890))
  :bind (:map copilot-completion-map
              ;; ("<tab>" . 'copilot-accept-completion)
              ;; ("TAB" . 'copilot-accept-completion)
	      ("C-e" . 'copilot-accept-completion)
              ;; ("C-TAB" . 'copilot-accept-completion-by-word)
              ;; ("C-<tab>" . 'copilot-accept-completion-by-word)
	      ("M-f" . 'copilot-accept-completion-by-word)))

(provide 'init-copilot)

;;; init-copilot.el ends here
