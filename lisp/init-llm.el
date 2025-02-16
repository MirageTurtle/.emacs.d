;; init-llm.el -*- lexical-binding: t; -*-

(defun gptel-api-key ()
  (read-file-contents "~/Documents/secrets/my_new_api_key"))

(use-package gptel
  :custom
  (gptel-model 'gpt-4o-mini)
  :config
  (defun read-file-contents (file-path)
    "Read the contents of FILE-PATH and return it as a string."
    (with-temp-buffer
      (insert-file-contents file-path)
      (buffer-string)))
  (setq
   gptel-backend (gptel-make-openai "Personal"
                   :stream t
                   :key #'gptel-api-key
		   :host "100.64.0.1:3002"
		   :protocol "http"
		   :models '(gpt-4o-mini deepseek-ai/DeepSeek-R1 deepseek-ai/DeepSeek-V3 o3-mini)
		   :endpoint "/v1/chat/completions"
		   :stream t)))

(use-package aider
  :straight (:host github :repo "tninja/aider.el" :files ("aider.el"))
  :config
  (setq aider-args '("--no-auto-commits"
		     "--model"
		     "openai/gpt-4o-mini"))
  (setenv "OPENAI_API_BASE" "http://100.64.0.1:3002/v1")
  (setenv "OPENAI_API_KEY" (gptel-api-key)))


(provide 'init-llm)

;;; init-llm.el ends here
