;;; init-emigo.el --- Emigo configuration -*- lexical-binding: t; -*-
;;; Code:

(use-package emigo
  :straight (:host github :repo "MatthewZMD/emigo" :files (:defaults "*.el" "*.py") :build (:not compile))
  :config
  (emigo-enable)
  :custom
  (emigo-api-key (mt/read-file-content "~/Documents/secrets/my_uni_api_key"))
  (emigo-model "openai/deepseek-ai/DeepSeek-V3")
  (emigo-base-url "http://100.64.0.1:3003/v1")
  ;; Securely load your API key (replace with your preferred method)
  (emigo-python-command "~/Documents/venv/emacs-python/bin/python3"))

(provide 'init-emigo)
