;;; init-interface.el --- Interface settings -*- lexical-binding: t -*-
;;; Commentary:

;; Including Fonts, Color showing, Miscellany.

;;; Code:

(use-package cnfonts
  :ensure t
  :init
  (cnfonts-mode 1)
  :config
  (setq use-default-font-for-symbols nil)
  ;; (setq cnfonts-use-face-font-rescale t)
  (define-key cnfonts-mode-map (kbd "C--") #'cnfonts-decrease-fontsize)
  (define-key cnfonts-mode-map (kbd "C-=") #'cnfonts-increase-fontsize)
  (setq cnfonts-profiles '("Program" "Document"))
  (setq cnfonts-personal-fontnames '(("JetBrains Mono" "LXGW WenKai Mono" "Maple Mono") ("LXGW WenKai Mono"))))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package dashboard
  :ensure t
  :config
  (setq dashboard-banner-logo-title "Enjoy Hacking!")
  (setq dashboard-projects-backend 'project-el)
  (setq dashboard-startup-banner 'logo)
  ;; (setq dashboard-startup-banner nil)
  ;; I actually not good at using bookmarks and agenda, so I remove them temporarily.
  (setq dashboard-items '((recents  . 10)
			  ;; (bookmarks . 5)
			  ;; (agenda . 5)
			  (projects . 10)))
  (dashboard-setup-startup-hook))

;; Smooth scroll up & down
(setq pixel-scroll-precision-interpolate-page t)
(add-hook 'after-init-hook #'pixel-scroll-precision-mode)

(defun pixel-scroll-down (&optional lines)
  (interactive)
  (if lines
      (pixel-scroll-precision-interpolate (* -1 lines (pixel-line-height)))
    (pixel-scroll-interpolate-down)))

(defun pixel-scroll-up (&optional lines)
  (interactive)
  (if lines
      (pixel-scroll-precision-interpolate (* lines (pixel-line-height))))
  (pixel-scroll-interpolate-up))

(defalias 'scroll-up-command 'pixel-scroll-interpolate-down)
(defalias 'scroll-down-command 'pixel-scroll-interpolate-up)

(provide 'init-interface)

;;; init-interface.el ends here
