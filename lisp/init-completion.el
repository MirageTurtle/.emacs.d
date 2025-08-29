;;; completion.el -*- lexical-binding: t; -*-

(use-package all-the-icons
  :straight t
  :if (display-graphic-p))

(use-package all-the-icons-completion
  :straight t
  :requires all-the-icons
  :if (display-graphic-p)
  :hook (after-init . all-the-icons-completion-mode))

(use-package dabbrev
  :config
  (setq dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

(use-package vertico
  :straight (:files (:defaults "extensions/*.el"))
  :bind (:map vertico-map
              ("TAB" . minibuffer-complete)
              ("<tab>" . minibuffer-complete)
              ("C-<return>" . vertico-insert)
              ("C-, ." . vertico-quick-jump))
  :hook ((after-init . vertico-mode))
  :after (consult)
  :defines (crm-separator)
  :config
  (setq vertico-cycle t
        vertico-resize nil
        vertico-count 15)

  (defadvice! +vertico--set-crm-separator-a (args)
    :filter-args #'completing-read-multiple
    (cons (concat "[CRM"
                  (replace-regexp-in-string "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                                            crm-separator)
                  "] "
                  (car args))
          (cdr args)))
  ;; WORKAROUND: https://github.com/minad/vertico#problematic-completion-commands
  (setq org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil)
  )

(use-package consult
  :ensure t
  :bind (("C-x b" . consult-buffer)
	 ;; ("C-x C-r" . consult-recent-file)
	 ;; ("C-x C-f" . consult-find)
	 ("M-y" . consult-yank-pop)
	 ;; ("C-c M-g" . consult-goto-line)
	 ("C-s" . consult-line)
	 :map minibuffer-local-map
	 ("C-r" . consult-history))
  :hook (after-init . consult-mode)
  :config
  (setq consult-narrow-key "<")
  (setq consult-preview-key 'any)
  (setq completion-styles '(substring basic partial-completion emacs22)))

;; corfu
(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  :custom
  (corfu-auto t) ;; enable auto completion
  (corfu-cycle t) ;; enable cycling for `corfu-next' and `corfu-previous'
  (corfu-preview-current nil) ;; disable current candidate preview
  (corfu-preselect 'prompt)   ;; preselect the prompt
  (corfu-on-exact-match nil) ;; do not complete on exact match
  (corfu-scroll-margin 10)     ;; set scroll margin
  )

;; orderless
(use-package orderless
  :demand t
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; marginalia
(use-package marginalia
  :ensure t
  :hook (vertico-mode . marginalia-mode)
  :bind (:map minibuffer-local-map
	      ("M-a" . marginalia-cycle)))

(provide 'init-completion)
