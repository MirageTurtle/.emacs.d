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
  (setq cnfonts-personal-fontnames '(("JetBrains Mono" "LXGW WenKai Mono") ("LXGW WenKai Mono"))))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package dashboard
 :ensure t
 :config
 (setq dashboard-banner-logo-title "Enjoy Hacking!")
 ;; (setq dashboard-projects-backend 'projectile) ; I have given up projectile.
 (setq dashboard-startup-banner 'logo)
 ;; (setq dashboard-startup-banner nil)
 ;; I actually not good at using bookmarks and agenda, so I remove them temporarily.
 (setq dashboard-items '((recents  . 10)
		  ;; (bookmarks . 5)
		  ;; (agenda . 5)
		  (projects . 10)))
 (dashboard-setup-startup-hook))

(provide 'init-interface)

;;; init-interface.el ends here
