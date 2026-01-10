;;; init-llm.el -*- lexical-binding: t; -*-

(require 'auth-source)

(defun mt/gptel-backend-plist-from-authinfo (machine)
  "Return a plist usable by `gptel-make-*' from authinfo MACHINE.

Authinfo fields mapping:
  user  → :host
  password      → :key"
  (let* ((auth-sources '("~/.authinfo.gpg"))
         (auth (car (auth-source-search
                     :machine machine
                     :max 1
                     :require '(:user :secret)))))
    (unless auth
      (error "No authinfo entry for machine %S" machine))
    (let ((plist (list
                  :host (plist-get auth :user)
                  :key  (funcall (plist-get auth :secret)))))
      (when-let ((endpoint (plist-get auth :port)))
        (setq plist (plist-put plist :endpoint endpoint)))
      plist)))

(use-package gptel
  :straight (gptel :type git :host github :repo "karthink/gptel")
  :defer t
  :custom
  (gptel-backend
   (apply #'gptel-make-openai
          "claude-sonnet-4-20250514-magit"
          :endpoint "/v1/chat/completions"
          :models '(claude-sonnet-4-20250514)
          :stream t
          (mt/gptel-backend-plist-from-authinfo "claude-sonnet-4-20250514-magit"))))

(provide 'init-llm)
;;; init-llm.el ends here
