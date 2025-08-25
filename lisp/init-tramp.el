;;; init-tramp.el -- Tramp configuration -*- lexical-binding: t; -*-

;; https://www.gnu.org/software/tramp/#Frequently-Asked-Questions-1
;; (setq tramp-verbose 1)
(setq tramp-default-method "scpx")
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))
(setq vc-handled-backends '(Git))

(connection-local-set-profile-variables
 'tramp-dired-profile
 '((dired-check-symlinks . nil)))
(connection-local-set-profiles
 '(:application tramp :protocol "scp")
 'tramp-dired-profile)

;; https://coredumped.dev/2025/06/18/making-tramp-go-brrrr./
(setq remote-file-name-inhibit-locks t ; Assume different Emacs sessions are not modifying the same remote file.
      tramp-use-scp-direct-remote-copying t
      remote-file-name-inhibit-auto-save-visited t)
(setq tramp-copy-size-limit (* 1024 1024) ;; 1MB
      tramp-verbose 2)
(connection-local-set-profile-variables
 'remote-direct-async-process
 '((tramp-direct-async-process . t)))
(connection-local-set-profiles
 '(:application tramp :protocol "scp")
 'remote-direct-async-process)
(with-eval-after-load 'tramp
  (with-eval-after-load 'compile
    (remove-hook 'compilation-mode-hook #'tramp-compile-disable-ssh-controlmaster-options)))

(setq tramp-ssh-controlmaster-options nil)
(setq tramp-use-connection-share nil) ; use the ssh config file for connections

(provide 'init-tramp)
