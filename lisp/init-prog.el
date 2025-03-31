;;; init-prog.el -*- lexical-binding: t; -*-
;;; Commentary:

;;; Log:

;; 2025-02-16:
;;   remove ts-fold, cause I try to use hs-minor-mode instead

;; Programming related configurations

;;; Code:

(add-hook 'prog-mode-hook #'show-paren-mode) ; highlight electric pair in program mode
(add-hook 'prog-mode-hook #'hs-minor-mode) ; fold code block in program mode

(use-package reformatter
  :ensure t)

;; debug
(use-package dape
  ;; require jsonrpc >= 1.0.24, I use 1.0.25
  :ensure t
  :config
  (setq dape-buffer-window-arrangement 'right))

;; eglot
(use-package eglot
  :hook
  ((json-mode jsonc-mode) . eglot-ensure)
  ((js2-mode typescript-mode) . eglot-ensure)
  ((python-mode) . eglot-ensure))

;; flymake
(use-package flymake
  :bind
  (:map flymake-mode-map
	("M-s-j" . flymake-goto-next-error)
	("M-s-k" . flymake-goto-prev-error)))

;; set the default mode for some file types
;; for every file type, if it is not in auto-mode-alist,
;; it will be added to auto-mode-alist
(defun mt/set-default-mode-if-not-exist (ext mode)
  "Set the default mode for file with extension EXT to MODE if it is not in auto-mode-alist."
  (unless (assoc ext auto-mode-alist)
    (add-to-list 'auto-mode-alist (cons ext mode)))
    (message "Set default mode for %s to %s" ext mode))

(defvar mt/default-mode-alist
  (list
   '("\\.js\\'" . js2-mode)
   '("\\.jsx\\'" . js2-mode)
   '("\\.ts\\'" . typescript-mode)
   '("\\.tsx\\'" . typescript-mode)
   '("\\.json\\'" . json-mode)
   '("\\.jsonc\\'" . jsonc-mode)
   '("\\.html\\'" . web-mode)
   '("\\.css\\'" . css-mode)
   '("\\.scss\\'" . scss-mode)
   '("\\.sass\\'" . sass-mode)
   '("\\.md\\'" . markdown-mode)
   '("\\.markdown\\'" . markdown-mode)
   '("\\.py\\'" . python-mode)
   '("\\.el\\'" . emacs-lisp-mode)
   '("\\.sh\\'" . shell-script-mode)
   '("\\.yaml\\'" . yaml-mode)
   '("\\.yml\\'" . yaml-mode)
   '("\\.toml\\'" . toml-mode)
   '("\\.org\\'" . org-mode)
   '("\\.go\\'" . go-mode)
   '("\\.rs\\'" . rust-mode)
   '("\\.lua\\'" . lua-mode)
   '("\\.sql\\'" . sql-mode))
  "List of default mode for file types.")

(dolist (pair mt/default-mode-alist)
  (let ((ext (car pair))
	(mode (cdr pair)))
    (mt/set-default-mode-if-not-exist ext mode)))

(provide 'init-prog)

;;; init-prog.el ends here
