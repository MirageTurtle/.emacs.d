;;; init-bash.el --- Bash configuration -*- lexical-binding: t -*-
;;; Commentary:

;;; Log:
;; 2025-04-03:
;;   * remove `bash-format-on-save-mode' because of apheleia
;;   * use `bash-ts-mode' instead of `sh-mode'

;;; Code:

(require 'init-prog)

;; (setq sh-basic-offset 4) ; set sh basic indent to 4 spaces (default is 4)

;; set shell-script-mode to use bash-ts-mode
(add-to-list 'major-mode-remap-alist '(sh-mode . bash-ts-mode))
(add-to-list 'major-mode-remap-alist '(shell-script-mode . bash-ts-mode))

(provide 'init-bash)

;;; init-bash.el ends here
