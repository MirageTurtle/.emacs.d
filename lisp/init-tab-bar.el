;;; init-tab-bar.el --- tab-bar configuration -*- lexical-binding: t; -*-

(use-package tab-bar
  :ensure nil
  :hook (window-setup . tab-bar-mode)
  :custom
  ;; use super + number to switch tab
  (tab-bar-select-tab-modifiers '(super))
  (tab-bar-tab-hints t)
  (tab-bar-close-button-show nil)
  (tab-bar-new-tab-choice "*dashboard*")
  (tab-bar-tab-name-truncated-max 20)
  (tab-bar-auto-width nil)
  :config
  ;; https://emacs-china.org/t/tab-bar/26008
  ;; Original Author: Roife
  ;; 给 tab 两边加上空格，更好看
  (setq tab-bar-tab-name-format-function
        (lambda (tab i)
          (let ((face (funcall tab-bar-tab-face-function tab)))
            (concat
             (propertize " " 'face face)
             (propertize (number-to-string i) 'face `(:inherit ,face :weight ultra-bold :underline t))
             (propertize (concat " " (alist-get 'name tab) " ") 'face face)))))
  )

(provide 'init-tab-bar)
;;; init-tab-bar.el ends here
