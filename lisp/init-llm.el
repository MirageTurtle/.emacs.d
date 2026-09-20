;;; init-llm.el -*- lexical-binding: t; -*-

(require 'auth-source)

(defun mt/authinfo-secret (machine)
  "Return the secret stored for authinfo MACHINE."
  (let* ((auth-sources '("~/.authinfo.gpg"))
         (auth (car (auth-source-search
                     :host machine
                     :max 1
                     :require '(:secret)))))
    (unless auth
      (error "No authinfo entry for machine %S" machine))
    (plist-get auth :secret)))

(defun mt/make-vertsineu-qwen-provider (key)
  "Create the VertSineu Qwen provider using KEY."
  (make-llm-openai-compatible
   :url "https://api.vertsineu.top/v1/"
   :key key
   :chat-model "qwen3.8-27b"
   :default-chat-non-standard-params
   '(("chat_template_kwargs"
      . ((enable_thinking . :false)
         (terse . :false))))))

(defun mt/llm-make-developer-prompt (developer-prompt user-prompt)
  "Create an LLM prompt from DEVELOPER-PROMPT and USER-PROMPT."
  (make-llm-chat-prompt
   :interactions
   (list (make-llm-chat-prompt-interaction
          :role 'developer
          :content developer-prompt)
         (make-llm-chat-prompt-interaction
          :role 'user
          :content user-prompt))))

(defvar mt/llm-commit-provider nil
  "LLM provider used to generate commit messages.")

(use-package llm
  :straight (:type git :host github :repo "ahyatt/llm")
  :defer t
  :init
  (setq llm-warn-on-nonfree nil)
  :config
  (require 'llm-openai)
  (setq mt/llm-commit-provider
        (mt/make-vertsineu-qwen-provider
         (mt/authinfo-secret "vertsineu-qwen-magit"))))

(provide 'init-llm)
;;; init-llm.el ends here
