;;; init-ivy.el --- Ivy configuration -*- lexical-binding: t -*-
;;; Commentary:

;; ivy is a very powerful package written by abo-abo.
;; It is a completion framework, especially for minibuffer completion.
;; I find a Chinese ivy tutorial:
;; https://emacs-china.org/t/ivy/12091
;; There are many ivy functions I do not know, even now.

;;; Code:

(use-package counsel
  :ensure t)

(use-package ivy
  :ensure t
  :after counsel
  :init
  :config
  (counsel-mode 1)
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq search-default-mode #'char-fold-to-regexp)
  (setq ivy-count-format "(%d/%d) ")
  :bind
  (("M-x" . 'counsel-M-x)
   ("C-x C-f" . 'counsel-find-file)
   ("C-s" . 'swiper)
   ("C-x b" . 'ivy-switch-buffer)
   ("C-c v" . 'ivy-push-view)
   ("C-c s" . 'ivy-switch-view)
   ("C-c V" . 'ivy-pop-view)
   ("C-x SPC" . 'counsel-mark-ring)
   :map minibuffer-local-map
   ("C-r" . counsel-minibuffer-history)
   ;; ~ivy-immediate-done~ is defaultly binded to ~C-M-j~
   ;; ~ivy-dispatching-done~ is defaultly binded to ~M-o~
   ;; ~ivy-call~ is defaultly binded to ~C-M-m~
   ))

(use-package ivy-avy
  :ensure t
  :after (ivy avy)
  :bind
  (:map ivy-minibuffer-map
	("C-;" . ivy-avy)))

(provide 'init-ivy)

;;; init-ivy.el ends here.
