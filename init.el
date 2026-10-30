;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into a number of other files.

;;; Code:
;; 30.1: `tab-bar-mode-map' (used in `init-tab-bar') was introduced in Emacs 30.1.
(let ((minver "30.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
;; (require 'init-benchmarking) ;; Measure startup time

(defconst *spell-check-support-enabled* t) ;; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-a-linux* (eq system-type 'gnu/linux))

;; GC is disabled during startup in `early-init.el'; restore the threshold
;; once idle so the first collection runs after the frame is drawn.
;; Perhaps consider to use `gcmh' package
(add-hook 'emacs-startup-hook
          (lambda ()
            (run-with-idle-timer 1 nil
                                 (lambda () (setq gc-cons-threshold (* 20 1024 1024))))))

(setq confirm-kill-emacs #'y-or-n-p) ; confirm if kill emacs
(electric-pair-mode t) ; auto electric pair((){}[] .ete)
(column-number-mode t) ; show column number in mode line
(global-auto-revert-mode t) ; emacs fresh buffer automatically if the file is edited in another place
(delete-selection-mode t) ; replace text in selection mode
(setq inhibit-startup-message t) ; no emacs hello face
(setq initial-major-mode 'fundamental-mode) ; keep prog-mode hooks (copilot, lsp-bridge, ...) off *scratch*
(setq make-backup-files nil) ; no backup files
(global-display-line-numbers-mode -1) ; show column number in window
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1)) ; no tool bar (void in --without-x builds)
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1)) ; no menu bar
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
(if *is-a-mac*
    (progn
      (setq-default process-connection-type nil)
      (setq-default magit-process-connection-type nil))
  ())



(require 'init-network)
(require 'init-straight)
(require 'init-basic)

;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(package-selected-packages
;;    '(lsp-bridge yasnippet copilot cnfonts org-bullets eglot lua-mode
;;                 pyvenv lsp-pyright lsp-ui yaml-pro json-mode
;;                 multiple-cursors smart-tabs-mode wgrep lsp-treemacs
;;                 lsp-ivy lsp-mode flycheck company treemacs-projectile
;;                 treemacs counsel-projectile projectile undo-tree
;;                 google-this rainbow-delimiters dashboard mwim counsel
;;                 ivy use-package gnu-elpa-keyring-update)))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(fringe ((t nil))))


(require 'init-env)
(require 'init-utils)
(require 'init-tramp)
(require 'init-efficiency)
(require 'init-completion)
(require 'init-emoji)
;; (require 'init-ivy)
(require 'init-meow)
(require 'init-search)
(require 'init-tab-bar)
(require 'init-dired)
(require 'init-ispell)

(require 'init-llm)
(require 'init-magit) ; init-magit requires init-llm
(require 'init-theme)
(require 'init-interface)
;; (require 'init-dirvish)

(require 'init-treesit)

(require 'init-prog)
(require 'init-bash)
(require 'init-copilot)
(require 'init-c)
(require 'init-docker)
(require 'init-go)
(require 'init-json)
(require 'init-lua)
(require 'init-markdown)
(require 'init-nix)
;; (require 'init-org)
(require 'init-org-gtd)
(require 'init-python)
(require 'init-rust)
(require 'init-typst)
(require 'init-yaml)
(require 'init-beancount)
(require 'init-mermaid)
(require 'init-lsp-bridge)

;; (require 'init-bib)

;; (require 'init-telega)

;; (require 'init-eaf)

(provide 'init)

;;; init.el ends here.
