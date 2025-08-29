;;; init-dired.el --- Dired configuration -*- lexical-binding: t; -*-

;; remove associated buffer when file is deleted
(defun mt/dired-kill-before-delete (file &rest rest)
  "Kill the buffer associated with FILE before it is deleted."
  (when-let ((buf (get-file-buffer file)))
    ;; ask user for confirmation
    (when (y-or-n-p (format "Kill buffer %s? " (buffer-name buf)))
      (kill-buffer buf))))
(advice-add 'dired-delete-file :before #'mt/dired-kill-before-delete)

(provide 'init-dired)
;;; init-dired.el ends here
