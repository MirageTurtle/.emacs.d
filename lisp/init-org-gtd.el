;;; init-org-gtd.el --- org-mode-based GTD configuration -*- lexical-binding: t; -*-
;;; Original article: https://web.archive.org/web/20170916093203/http://emacs.cafe/emacs/orgmode/gtd/2017/06/30/orgmode-gtd.html
;;; Original article (Chinese): https://grass.show/translate/do-gtd-in-orgmode/

(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook 'visual-line-mode)


(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c C-w") 'org-refile)

(global-set-key (kbd "C-c a") 'org-agenda)
(setq org-agenda-start-on-weekday 0) ;; start on Sunday

(setq org-agenda-files '("~/gtd/inbox.org"
                         "~/gtd/gtd.org"
                         "~/gtd/tickler.org"))

(setq org-capture-templates '(("t" "Todo [inbox]" entry
                               (file+headline "~/gtd/inbox.org" "Tasks")
                               "* TODO %i%?")
                              ("T" "Tickler" entry
                               (file+headline "~/gtd/tickler.org" "Tickler")
                               "* %i%?\n %U")))

(setq org-refile-targets '(("~/gtd/gtd.org" :maxlevel . 3)
                           ("~/gtd/someday.org" :level . 1)
                           ("~/gtd/tickler.org" :maxlevel . 2)))

(setq org-todo-keywords '((sequence "TODO(t)" "WAIT(w)" "|" "DONE(d)" "CANC(c)")))

;; ..., =...=, inline-src, src block (multiline too)
(require 'org)
(require 'org-element)
(defun mt/org-copy-command-at-point ()
  "Copy contents of verbatim, code, inline-src or src block at point."
  (interactive)
  (let* ((el (org-element-context))
         (type (org-element-type el))
         (val (and (memq type '(verbatim code inline-src-block src-block))
                   (org-element-property :value el))))
    (if val
        (progn (kill-new val) (message "Copied: %s" val))
      (user-error "Put point inside ..., =...=, inline-src, or src block"))))

;; Set indentation of code blocks to zero
(setq org-edit-src-content-indentation 0)

(provide 'init-org-gtd)
;;; init-org-gtd.el ends here
