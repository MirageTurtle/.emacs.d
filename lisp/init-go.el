;;; init-go.el --- Go configuration -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(require 'init-prog)

(use-package go-mode
  :ensure t
  :hook
  (go-mode . go-format-on-save-mode)
  :config
  (reformatter-define go-format
    :program "gofmt"
    :args '("-s"))
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode)))


;; ;; dape part
;; ;; copy from https://emacs-china.org/t/dape-el-lsp-mode-dap/25784/8
;; (defun vmacs-dape--select-go-args ()
;;   (require 'which-func)
;;   (if (string-suffix-p "_test.go"   (buffer-name))
;;       (when-let* ((test-name (which-function))
;;                   (test-regexp (concat "^" test-name "$")))
;;         (if test-name
;;             `["-test.run" ,test-regexp]
;;           (error "No test selected")))
;;     (if  current-prefix-arg
;;         (vconcat (split-string (read-shell-command "args: " nil
;;                                                    (if (equal (car compile-history) "")
;;                                                        '(compile-history . 1)
;;                                                      'compile-history))))
;;       [])))

;; ;; https://github.com/go-delve/delve/blob/master/Documentation/usage/dlv_dap.md
;; (defun vmacs-dape-test-p ()
;;   (if (string-suffix-p "_test.go"   (buffer-name))
;;       "test" "debug"))

;; (defun vmacs-dape-relative-dir ()
;;   "Return the file directory relative to dape's cwd. This is used by Delve debugger."
;;   (if (string-suffix-p "_test.go"   (buffer-name))
;;       (concat "./" (file-relative-name
;;                     default-directory (funcall dape-cwd-fn)))
;;     (funcall dape-cwd-fn)))

;; ;; inside your dape-config
;; (add-to-list 'dape-configs
;;              `(delve
;;                modes (go-mode go-ts-mode)
;;                command "dlv"
;;                command-cwd dape-cwd-fn
;;                command-args ("dap" "--listen" "127.0.0.1:55878")
;;                host "127.0.0.1"
;;                port 55878
;;                :type "go"
;;                :name "go-debug"
;;                :request "launch"
;;                :mode vmacs-dape-test-p
;;                :cwd dape-cwd-fn
;;                :program vmacs-dape-relative-dir
;;                :args vmacs-dape--select-go-args))

(provide 'init-go)

;;; init-go.el ends here.
