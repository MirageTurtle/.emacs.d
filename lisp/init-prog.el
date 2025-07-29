;;; init-prog.el -*- lexical-binding: t; -*-
;;; Commentary:

;;; Log:

;; 2025-02-16:
;;   * remove ts-fold, cause I try to use hs-minor-mode instead
;; 2025-04-04:
;;   * add `eldoc-box'
;;   * use `eldoc-box-hover-mode' instead of `eldoc-box-hover-at-point-mode'
;;     since I think the right part of the screen is blank at most time,
;;     and showing the doc at the right top could help me to move my neck :)

;; Programming related configurations

;;; Code:

(use-package jsonrpc
  :straight (:type built-in))

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
  :ensure t
  :after jsonrpc
  :config
  (add-to-list 'eglot-server-programs '(nix-mode . ("nil")))
  :hook
  ((json-mode jsonc-mode) . eglot-ensure)
  ((js2-mode typescript-mode) . eglot-ensure)
  ((python-ts-mode) . eglot-ensure)
  ((sh-mode bash-ts-mode) . eglot-ensure)
  ((rust-mode) . eglot-ensure)
  ((go-mode) . eglot-ensure)
  ((c-mode) . eglot-ensure)
  ((nix-mode) . eglot-ensure))

;; eldoc-box
(use-package eldoc-box
  :straight (:type git :host github :repo "casouri/eldoc-box")
  :ensure t
  :hook
  (eglot-managed-mode . eldoc-box-hover-mode)
  :config
  (setq eldoc-box-show-with-cursor t)
  (setq eldoc-box-show-with-mouse t)
  (setq eldoc-box-border-color "#4f5b93")
  (setq eldoc-box-minibuffer t))

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


(defun mt/set-default-mode-even-if-exist (ext mode)
  "Set the default mode for file with extension EXT to MODE even if it is in auto-mode-alist."
  (let ((mode-function (if (stringp mode)
			   (intern mode)
			 mode)))
    (if (assoc ext auto-mode-alist)
	(setcdr (assoc ext auto-mode-alist) mode-function)
      (add-to-list 'auto-mode-alist (cons ext mode-function)))))

(defun mt/set-default-mode (ext mode)
  "Set the default mode for file with extension EXT to MODE."
  (interactive "sExtension: \nSMode: ")
  (setq ext (concat "\\." ext "\\'"))
  (mt/set-default-mode-even-if-exist ext mode))

(defvar mt/default-mode-alist
  (list
   '("\\.js\\'" . js2-mode)
   '("\\.jsx\\'" . js2-mode)
   '("\\.ts\\'" . typescript-mode)
   '("\\.tsx\\'" . typescript-mode)
   '("\\.json\\'" . json-mode)
   '("\\.jsonc\\'" . jsonc-mode)
   '("\\.html\\'" . html-mode)
   '("\\.css\\'" . css-mode)
   '("\\.scss\\'" . scss-mode)
   '("\\.sass\\'" . sass-mode)
   '("\\.md\\'" . markdown-mode)
   '("\\.markdown\\'" . markdown-mode)
   '("\\.py\\'" . python-ts-mode)
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

;; formatter package: apheleia
(use-package apheleia
  :straight t
  :config
  (apheleia-global-mode +1)
  (setq apheleia-remote-algorithm "remote")
  (setq apheleia-formatters-respect-indent-level nil))

(provide 'init-prog)

;;; init-prog.el ends here
