;;; init-efficiency.el --- Configuration for personal efficiency -*- lexical-binding: t -*-
;;; Commentary:

;; Perhaps it is a bit messy.

;;; Log:

;; 2025-02-15:
;;   remove hydra to an independent file;
;;   change the setting of ace-window with hydra;
;;   add yasnippet;

;;; Code:

(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-hl-line-mode 1) ;; highlight current line

(global-set-key (kbd "M-n") 'scroll-up-command)
(global-set-key (kbd "M-p") 'scroll-down-command)

;; amx is an alternative to smex
;; (use-package amx
;;   :straight t
;;   :config
;;   (amx-mode 1)
;;   )

;; Move Where I Mean
(use-package mwim
  :straight t
  :bind
  ("C-a" . mwim-beginning-of-code-or-line)
  ("C-e" . mwim-end-of-code-or-line))

;; customed split window function
(defun mt/split-window-right ()
  (interactive)
  (split-window-right)
  (other-window 1)
  (call-interactively 'switch-to-buffer))
(defun mt/split-window-below ()
  (interactive)
  (split-window-below)
  (other-window 1)
  (call-interactively 'switch-to-buffer))
(global-set-key (kbd "C-x 2") 'mt/split-window-below)
(global-set-key (kbd "C-x 3") 'mt/split-window-right)

;; multi-cursor
;; https://emacs-china.org/t/meow/15679 says meow could replace multiple-cursor,
;; so I may remove it in the future.
(use-package multiple-cursors
  :straight t
  :bind
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-c C-<" . mc/mark-all-like-this))

;; ace-window
;; ace-window is a very great package to switch windows.
(use-package ace-window
  :straight t
  :bind
  ("C-x o" . ace-window))
;; https://github.com/abo-abo/oremacs/blob/a24a45f079a0eaa7f584bc32ce3b33a545f29ff7/keys.el#L256-L287
(use-package hydra
  :straight t
  :config
  (defhydra hydra-window (:color red
                                 :columns nil)
    "window"
    ("h" windmove-left nil)
    ("j" windmove-down nil)
    ("k" windmove-up nil)
    ("l" windmove-right nil)
    ("H" hydra-move-splitter-left nil)
    ("J" hydra-move-splitter-down nil)
    ("K" hydra-move-splitter-up nil)
    ("L" hydra-move-splitter-right nil)
    ("v" (lambda ()
           (interactive)
           (split-window-right)
           (windmove-right))
     "vert")
    ("x" (lambda ()
           (interactive)
           (split-window-below)
           (windmove-down))
     "horz")
    ("t" transpose-frame "'" :exit t)
    ("o" delete-other-windows "one" :exit t)
    ("a" ace-window "ace")
    ("s" ace-swap-window "swap")
    ("d" ace-delete-window "del")
    ("i" ace-maximize-window "ace-one" :exit t)
    ("b" ido-switch-buffer "buf")
    ("m" headlong-bookmark-jump "bmk")
    ("q" nil "cancel")
    ("u" (progn (winner-undo) (setq this-command 'winner-undo)) "undo")
    ("f" nil))
  (global-set-key (kbd "M-o") 'hydra-window/body))

;; undo-tree
;; ~C-x u~ to open undo-tree-visualizer
(use-package undo-tree
  :straight t
  :init (global-undo-tree-mode)
  :diminish undo-tree-mode
  :custom
  (undo-tree-auto-save-history nil))

;; which-key
(use-package which-key
  :straight t
  :diminish which-key-mode
  :init (which-key-mode))

;; avy
(use-package avy
  :straight t
  :bind
  ;; I remove `avy-goto-char' because I can use `meow-find' to replace it.
  ;; And I use `avy-goto-line' more, so I bind it to `C-;'.
  ("C-'" . avy-goto-char-timer)
  ("C-;" . avy-goto-line))


(use-package yasnippet
  :straight t
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1))


(provide 'init-efficiency)

;;; init-efficiency.el ends here.
