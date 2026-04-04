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
  :straight t
  :bind (("C-x b" . consult-buffer)
	 ;; ("C-x C-r" . consult-recent-file)
	 ;; ("C-x C-f" . consult-find)
	 ("M-y" . consult-yank-pop)
	 ;; ("C-c M-g" . consult-goto-line)
	 ("C-s" . consult-line)
	 :map minibuffer-local-map
	 ("C-r" . consult-history))
  :config
  (setq consult-narrow-key "<")
  (setq consult-preview-key 'any)
  (setq completion-styles '(orderless)))

;; orderless
(use-package orderless
  :demand t
  :straight t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; marginalia
;; (use-package marginalia
;;   :straight t
;;   :hook (vertico-mode . marginalia-mode)
;;   :bind (:map minibuffer-local-map
;; 	      ("M-a" . marginalia-cycle)))

(provide 'init-completion)
