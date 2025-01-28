;; init-llm.el -*- lexical-binding: t; -*-

(use-package gptel
  :custom
  (gptel-model 'gpt-4o-mini)
  :config
  (defun read-file-contents (file-path)
    "Read the contents of FILE-PATH and return it as a string."
    (with-temp-buffer
      (insert-file-contents file-path)
      (buffer-string)))
  (defun gptel-api-key ()
    (read-file-contents "~/Documents/secrets/my_new_api_key"))
  (setq
   gptel-backend (gptel-make-openai "GPT-4o-mini"
                   :stream t
                   :key #'gptel-api-key
		   :host "100.64.0.1:3002"
		   :protocol "http"
		   :models '(gpt-4o-mini)
		   :endpoint "/v1/chat/completions"
		   :stream t)))

(provide 'init-llm)

;;; init-llm.el ends here
