;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into a number of other files.

;;; Code:
(let ((minver "25.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version< emacs-version "26.1")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
;; (require 'init-benchmarking) ;; Measure startup time

(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-a-linux* (eq system-type 'gnu/linux))

;; Adjust garbage collection thresholds during startup
;; Perhaps consider to use `gcmh' package
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

(setq confirm-kill-emacs #'y-or-n-p) ; confirm if kill emacs
(electric-pair-mode t) ; auto electric pair((){}[] .ete)
(column-number-mode t) ; show column number in mode line
(global-auto-revert-mode t) ; emacs fresh buffer automatically if the file is edited in another place
(delete-selection-mode t) ; replace text in selection mode
(setq inhibit-startup-message t) ; no emacs hello face
(setq make-backup-files nil) ; no backup files
(global-display-line-numbers-mode 1) ; show column number in window
(tool-bar-mode -1) ; no tool bar
(menu-bar-mode -1) ; no menu bar
(when (not (display-graphic-p)) (tab-bar-mode -1))
(when (display-graphic-p) (toggle-scroll-bar -1)) ; no scroll bar in graphic window
(savehist-mode 1) ; save buffer history
(setq-default cursor-type 'bar)

;; Global Key bind
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-c C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c ;") 'comment-line)

;; for Chinese input
;; But I am trying to use the input method of Emacs,
;; so I comment these lines.
;; (global-set-key (kbd "M-《") 'beginning-of-buffer)
;; (global-set-key (kbd "M-》") 'end-of-buffer)

;; macOS preferences
(if *is-a-mac*
    (progn
      (setq mac-command-modifier 'super)
      (setq mac-option-modifier 'meta)
      (setq mac-control-modifier 'control)
      (setq mac-shift-modifier 'shift))
  ())
(if *is-a-mac*
    (progn
      (global-set-key (kbd "s-a") 'mark-whole-buffer)
      (global-set-key (kbd "s-c") 'kill-ring-save)
      (global-set-key (kbd "s-s") 'save-buffer)
      (global-set-key (kbd "s-v") 'yank)
      (global-set-key (kbd "s-z") 'undo)
      (global-set-key (kbd "s-x") 'kill-region))
  ())


;; (setq package-install-upgrade-built-in t)

;; package
;; (require 'package)
(setq package-archives '(("gnu" . "https://mirrors.ustc.edu.cn/elpa/gnu/")
                         ("melpa" . "https://mirrors.ustc.edu.cn/elpa/melpa/")
                         ("nongnu" . "https://mirrors.ustc.edu.cn/elpa/nongnu/")))
(setq package-check-signature nil)
;; (setq package-check-signature 'allow-unsigned)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
;; (package-initialize)
;; (package-refresh-contents)

;; straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)
;; (straight-pull-all)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(lsp-bridge yasnippet copilot cnfonts org-bullets org-roam eglot lua-mode pyvenv lsp-pyright lsp-ui yaml-pro json-mode multiple-cursors smart-tabs-mode wgrep lsp-treemacs lsp-ivy lsp-mode flycheck company treemacs-projectile treemacs counsel-projectile projectile undo-tree google-this rainbow-delimiters dashboard mwim counsel ivy use-package gnu-elpa-keyring-update)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;; '(org-level-1 ((t (:inherit outline-1 :height 1.728))))
 ;; '(org-level-2 ((t (:inherit outline-2 :height 1.44))))
 ;; '(org-level-3 ((t (:inherit outline-3 :height 1.2))))
 )

(eval-when-compile
  (require 'use-package))


(require 'init-env)
(require 'init-network)

(require 'init-utils)
(require 'init-efficiency)
(require 'init-emoji)
(require 'init-ivy)
(require 'init-llm)
(require 'init-meow)

(require 'init-git)
(require 'init-theme)
(require 'init-interface)
;; (require 'init-dirvish)

(require 'init-atomic-chrome)

(require 'init-lsp-bridge)
(require 'init-copilot)
(require 'init-tree-sitter)

(require 'init-prog)
(require 'init-python)
(require 'init-org)
(require 'init-lua)
(require 'init-json)
(require 'init-yaml)
(require 'init-rust)
(require 'init-go)
(require 'init-bash)

(require 'init-bib)

;; (require 'init-eaf)

(provide 'init)

;;; init.el ends here.
