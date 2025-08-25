;;; completion.el -*- lexical-binding: t; -*-

;; company
(use-package company
  :straight t
  :hook (after-init . global-company-mode)
  :config
  (setq company-tooltip-align-annotations t
        company-tooltip-limit 12
        company-idle-delay 0.1
        company-echo-delay (if (display-graphic-p) nil 0)
        company-minimum-prefix-length 1
	company-show-numbers t
        ;; company-icon-margin 3
        company-require-match nil
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
        company-global-modes '(not erc-mode message-mode help-mode
                                   gud-mode eshell-mode shell-mode)
        company-backends '((company-capf :with company-yasnippet)
                           (company-dabbrev-code company-keywords company-files)
                           company-dabbrev)
        ;; company-format-margin-function nil
        company-transformers '(company-sort-prefer-same-case-prefix
                               company-sort-by-occurrence
                               company-sort-by-backend-importance))
  )

;; company-box
(use-package company-box
  :straight t
  :if (display-graphic-p)
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-icons-alist 'company-box-icons-all-the-icons))

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

;; marginalia
(use-package marginalia
  :ensure t
  :hook (vertico-mode . marginalia-mode)
  :bind (:map minibuffer-local-map
	      ("M-a" . marginalia-cycle)))

(provide 'init-completion)
