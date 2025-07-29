;;; init-bash.el --- Bash configuration -*- lexical-binding: t -*-
;;; Commentary:

;;; Log:
;; 2025-04-03:
;;   * remove `bash-format-on-save-mode' because of apheleia
;;   * use `bash-ts-mode' instead of `sh-mode'

;;; Code:

(require 'init-prog)

;; (setq sh-basic-offset 4) ; set sh basic indent to 4 spaces (default is 4)

(unless (assoc 'bash treesit-language-source-alist)
  (push '(bash . ("https://github.com/tree-sitter/tree-sitter-bash")) treesit-language-source-alist))

;; set shell-script-mode to use bash-ts-mode
(setq major-mode-remap-alist
      '((sh-mode . bash-ts-mode)
	(shell-script-mode . bash-ts-mode)))

(provide 'init-bash)

;;; init-bash.el ends here
