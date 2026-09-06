;;; init-llm.el -*- lexical-binding: t; -*-

(require 'auth-source)

(defun mt/gptel-backend-plist-from-authinfo (machine)
  "Return a plist usable by `gptel-make-*' from authinfo MACHINE.

Authinfo fields mapping:
  machine       → lookup key
  login         → :host
  password      → :key"
  (let* ((auth-sources '("~/.authinfo.gpg"))
         (auth (car (auth-source-search
                     :host machine
                     :max 1
                     :require '(:user :secret)))))
    (unless auth
      (error "No authinfo entry for machine %S" machine))
    (let ((plist (list
                  :host (plist-get auth :user)
                  :key  (plist-get auth :secret))))
      (when-let ((endpoint (plist-get auth :port)))
        (setq plist (plist-put plist :endpoint endpoint)))
      plist)))

(use-package gptel
  :straight (gptel :type git :host github :repo "karthink/gptel")
  :defer t
  :custom
  (gptel-backend
   (apply #'gptel-make-openai
          "deepseek-magit"
          :endpoint "/chat/completions"
          :models '(deepseek-chat)
          :stream t
          (mt/gptel-backend-plist-from-authinfo "mt-deepseek-magit"))
   ;; (apply #'gptel-make-openai
   ;;        "vertsineu-qwen-magit"
   ;;        :endpoint "/v1/chat/completions"
   ;;        :models '(qwen3.8-reasoner)
   ;;        :stream t
   ;;        :request-params '(:chat_template_kwargs ((enable_thinking . :json-false)))
   ;;        (mt/gptel-backend-plist-from-authinfo "vertsineu-qwen-magit"))
   ))

(provide 'init-llm)
;;; init-llm.el ends here
