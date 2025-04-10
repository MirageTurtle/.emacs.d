;; init-llm.el -*- lexical-binding: t; -*-

(defun gptel-api-key ()
  (mt/read-file-contents "~/Documents/secrets/my_new_api_key"))

(defun gptel-uni-api-key()
  (mt/read-file-contents "~/Documents/secrets/my_uni_api_key"))

(use-package gptel
  :custom
  (gptel-model 'claude-3.7-sonnet)
  :config
  (setq
   gptel-backend (gptel-make-openai "Personal"
                   :stream t
                   ;; :key #'gptel-api-key
		   ;; :host "100.64.0.1:3002"
                   :key #'gptel-uni-api-key
		   :host "100.64.0.1:3003"
		   :protocol "http"
		   :models '(
			     ;; OpenAI
			     gpt-4o-mini
			     o1-mini
			     o3-mini
			     o3-mini-high
			     gpt-4o-mini-联网
			     o1-mini-联网
			     o3-mini-联网
			     o3-mini-high-联网
			     ;; DeepSeek
			     deepseek-ai/DeepSeek-R1
			     deepseek-ai/DeepSeek-V3
			     ;; deepseek-chat
			     ;; deepseek-reasoner
			     ;; deepseek-chat-联网
			     ;; deepseek-reasoner-联网
			     ;; Anthropic AI
			     claude-3.5-sonnet
			     claude-3.7-sonnet
			     claude-3.7-sonnet-thinking
			     claude-3.5-sonnet-联网
			     claude-3.7-sonnet-联网
			     claude-3.7-sonnet-thinking-联网
			     ;; Google
			     gemini-2.0-flash
			     gemini-2.0-pro
			     gemini-2.0-flash-联网
			     gemini-2.0-pro-联网
			     ;; Qwen
			     Qwen/QwQ-32B
			     )
		   :endpoint "/v1/chat/completions"
		   :stream t))
  (setq gptel-default-mode 'org-mode))

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
