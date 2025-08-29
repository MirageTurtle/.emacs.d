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
  (defun mt/tab-bar-tab-name-format-function (tab i)
    "Return the name for TAB with index I, prefixing a * when the current buffer is modified.
The star is shown only on the selected tab (cheap & reliable)."
    (let* ((face (funcall tab-bar-tab-face-function tab))
           ;; Selected tabs get the `tab-bar-tab` face; others get `tab-bar-tab-inactive`.
           (selected (eq face 'tab-bar-tab))
           (buf (and selected (window-buffer (selected-window))))
           (modified (and buf (buffer-modified-p buf)))
           (num (concat (if modified "*" "") (number-to-string i))))
      (concat
       (propertize " " 'face face)
       (propertize num 'face `(:inherit ,face :weight ultra-bold :underline t))
       (propertize (concat " " (alist-get 'name tab) " ") 'face face))))
  (setq tab-bar-tab-name-format-function #'mt/tab-bar-tab-name-format-function)
  )

(provide 'init-tab-bar)
;;; init-tab-bar.el ends here
