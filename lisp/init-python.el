;;; init-python.el --- Python -*- lexical-binding: t -*-
;;; Commentary:

;;; Log:

;; 2025-02-16:
;;   Remove black support
;; 2025-03-31:
;;   Add blacken for black
;; 2025-04-07:
;;   * Remove blacken support, because I use apheleia now.
;;   * add `python-mode' into `pyvenv-post-hook'.

;;; Code:

;; virtual env
(use-package pyvenv
  :ensure t
  :config
  (setenv "WORKON_HOME" (expand-file-name "~/Documents/venv"))
  ;; (setq python-shell-interpreter "python3")
  (pyvenv-mode t)
  (pyvenv-activate (expand-file-name "~/Documents/venv/base")))

;; for lsp-bridge
(defun local/lsp-bridge-get-single-lang-server-by-project (project-path filepath)
  (let* ((json-object-type 'plist)
         (custom-dir (expand-file-name ".cache/lsp-bridge/ruff" user-emacs-directory))
         (custom-config (expand-file-name "ruff.json" custom-dir))
         (default-config (json-read-file (expand-file-name "straight/build/lsp-bridge/langserver/ruff.json" user-emacs-directory)))
         (settings (plist-get default-config :settings))
         )

    (plist-put settings :pythonPath (executable-find "python3"))

    (make-directory (file-name-directory custom-config) t)

    (with-temp-file custom-config
      (insert (json-encode default-config)))

    custom-config))

;; (add-hook 'python-mode-hook (lambda () (setq-local lsp-bridge-get-single-lang-server-by-project 'local/lsp-bridge-get-single-lang-server-by-project)))

;; (add-hook 'pyvenv-post-activate-hooks
;;           (lambda ()
;;             (lsp-bridge-restart-process)))
(add-hook 'pyvenv-post-activate-hooks
	  (lambda ()
	    (eglot-ensure)
	    (python-mode)))

;; ein is a juptyer notebook client
(use-package ein
  :ensure t)

(add-to-list 'dape-configs
             `(debugpy
               modes (python-mode python-ts-mode)
               command "python3"
               command-args ("-m" "debugpy.adapter")
               :type "executable"
               :request "launch"
               :cwd dape-cwd
               :program dape-buffer-default))

;; formatter package: apheleia
(use-package apheleia
  :config
  (setf (alist-get 'python-mode apheleia-mode-alist) '(ruff))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(ruff)))

(provide 'init-python)

;;; init-python.el ends here.
