;;; init-copilot.el --- Copilot configuration -*- lexical-binding: t -*-
;;; Commentary:

;; editorconfig and jsonrpc are required by copilot

;;; Code:

(use-package editorconfig
  :straight t)

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :straight t
  :hook
  (prog-mode . mt/copilot-mode)
  :config
  (setq copilot-indent-offset-warning-disable t)
  (defun mt/copilot-mode ()
    "Enable copilot mode if the current buffer is not remote."
    (unless (file-remote-p default-directory)
      (copilot-mode)))
  :bind (:map copilot-completion-map
              ;; ("<tab>" . 'copilot-accept-completion)
              ;; ("TAB" . 'copilot-accept-completion)
	      ("C-e" . 'copilot-accept-completion)
              ;; ("C-TAB" . 'copilot-accept-completion-by-word)
              ;; ("C-<tab>" . 'copilot-accept-completion-by-word)
	      ("M-f" . 'copilot-accept-completion-by-word)))

(provide 'init-copilot)

;;; init-copilot.el ends here
