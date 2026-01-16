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

(use-package prog-mode
  :hook
  ((prog-mode . show-paren-mode) ; highlight electric pair in program mode
   ;; (prog-mode . hs-minor-mode) ; fold code block in program mode
   (prog-mode . display-line-numbers-mode) ; show line numbers in program mode
   (prog-mode . which-function-mode) ; show current function in mode line
   ))

(setq-default indent-tabs-mode nil) ; use space instead of tab

(use-package jsonrpc
  :straight (:type built-in))

(use-package reformatter
  :straight t)

;; debug
(use-package dape
  ;; require jsonrpc >= 1.0.24, I use 1.0.25
  :straight t
  :config
  (setq dape-buffer-window-arrangement 'right))

;; eglot
(use-package eglot
  ;; :straight t
  :straight (eglot :type git
                   :host nil
                   :repo "https://mirrors.ustc.edu.cn/emacs.git"
                   :files ("lisp/progmodes/eglot.el"))
  :after jsonrpc
  :custom-face (eglot-highlight-symbol-face ((t (:underline t :weight bold))))
  :bind (:map eglot-mode-map
              ("M-<return>" . eglot-code-actions))
  :config
  (add-to-list 'eglot-server-programs '(nix-mode . ("nil")))
  (defun mt/eglot-ensure ()
    "Ensure eglot is enabled if the current buffer is not remote."
    (unless (file-remote-p default-directory)
      (eglot-ensure)))
  :hook
  ((json-mode jsonc-mode) . mt/eglot-ensure)
  ((js2-mode typescript-mode) . mt/eglot-ensure)
  ((python-ts-mode) . mt/eglot-ensure)
  ((sh-mode bash-ts-mode) . mt/eglot-ensure)
  ((rust-mode) . mt/eglot-ensure)
  ((go-mode) . mt/eglot-ensure)
  ((c-mode) . mt/eglot-ensure)
  ((nix-mode) . mt/eglot-ensure))


;; eldoc-box
(use-package eldoc-box
  :straight (:type git :host github :repo "casouri/eldoc-box")
  :straight t
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
   '("\\.sql\\'" . sql-mode)
   '("\\.cu\\'" . c++-mode))
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
  (setq apheleia-formatters-respect-indent-level t)
  (setq apheleia-mode-lighter " Aphe"))

(provide 'init-prog)

;;; init-prog.el ends here
