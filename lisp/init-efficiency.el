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

(require 'init-hydra)

(global-hl-line-mode 1) ;; highlight current line

(global-set-key (kbd "M-n") 'scroll-up-command)
(global-set-key (kbd "M-p") 'scroll-down-command)

;; amx is an alternative to smex
(use-package amx
  :ensure t
  :config
  (amx-mode 1))

;; Move Where I Mean
(use-package mwim
  ;; :straight t
  :ensure t
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
  :ensure t
  :bind
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-c C-<" . mc/mark-all-like-this))

;; ace-window
;; ace-window is a very great package to switch windows.
(use-package ace-window
  :ensure t
  :bind
  ("C-x o" . ace-window))
;; https://github.com/abo-abo/oremacs/blob/a24a45f079a0eaa7f584bc32ce3b33a545f29ff7/keys.el#L256-L287
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
(global-set-key (kbd "M-o") 'hydra-window/body)

;; awesome-tab
;; TODO:
;; 1. show ace-window number
(use-package awesome-tab
  :straight (awesome-tab :type git :host github :repo "manateelazycat/awesome-tab")
  :ensure t
  :config
  (awesome-tab-mode t))
(defhydra awesome-fast-switch (:hint nil)
  "
 ^^^^Fast Move             ^^^^Tab                    ^^Search            ^^Misc
-^^^^--------------------+-^^^^---------------------+-^^----------------+-^^---------------------------
   ^_k_^   prev group    | _C-a_^^     select first | _b_ search buffer | _C-k_   kill buffer
 _h_   _l_  switch tab   | _C-e_^^     select last  | _g_ search group  | _C-S-k_ kill others in group
   ^_j_^   next group    | _C-j_^^     ace jump     | ^^                | ^^
 ^^0 ~ 9^^ select window | _C-h_/_C-l_ move current | ^^                | ^^
-^^^^--------------------+-^^^^---------------------+-^^----------------+-^^---------------------------
"
  ("h" awesome-tab-backward-tab)
  ("j" awesome-tab-forward-group)
  ("k" awesome-tab-backward-group)
  ("l" awesome-tab-forward-tab)
  ("0" my-select-window)
  ("1" my-select-window)
  ("2" my-select-window)
  ("3" my-select-window)
  ("4" my-select-window)
  ("5" my-select-window)
  ("6" my-select-window)
  ("7" my-select-window)
  ("8" my-select-window)
  ("9" my-select-window)
  ("C-a" awesome-tab-select-beg-tab)
  ("C-e" awesome-tab-select-end-tab)
  ("C-j" awesome-tab-ace-jump)
  ("C-h" awesome-tab-move-current-tab-to-left)
  ("C-l" awesome-tab-move-current-tab-to-right)
  ("b" ivy-switch-buffer)
  ("g" awesome-tab-counsel-switch-group)
  ("C-k" kill-current-buffer)
  ("C-S-k" awesome-tab-kill-other-buffers-in-current-group)
  ("q" nil "quit"))
;; winum users can use `winum-select-window-by-number' directly.
(defun my-select-window-by-number (win-id)
  "Use `ace-window' to select the window by using window index.
WIN-ID : Window index."
  (let ((wnd (nth (- win-id 1) (aw-window-list))))
    (if wnd
        (aw-switch-to-window wnd)
      (message "No such window."))))
(defun my-select-window ()
  (interactive)
  (let* ((event last-input-event)
         (key (make-vector 1 event))
         (key-desc (key-description key)))
    (my-select-window-by-number
     (string-to-number (car (nreverse (split-string key-desc "-")))))))
(global-set-key (kbd "C-x t") 'awesome-fast-switch/body)

;; undo-tree
;; ~C-x u~ to open undo-tree-visualizer
(use-package undo-tree
 :ensure t
 :init (global-undo-tree-mode)
 :custom
 (undo-tree-auto-save-history nil))

;; which-key
(use-package which-key
 :ensure t
 :init (which-key-mode))

;; avy
(use-package avy
  :ensure t
  :bind
  ;; I remove `avy-goto-char' because I can use `meow-find' to replace it.
  ;; And I use `avy-goto-line' more, so I bind it to `C-;'.
  ("C-'" . avy-goto-char-timer)
  ("C-;" . avy-goto-line))


(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(provide 'init-efficiency)

;;; init-efficiency.el ends here.
