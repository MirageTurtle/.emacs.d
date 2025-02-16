;; init-rss.el -*- lexical-binding: t; -*-

;; use elfeed to read rss feeds
(use-package elfeed
  :ensure t
  :config
  (setq elfeed-feeds
	'("https://mazzzystar.com/atom.xml"
	  "https://manateelazycat.github.io/feed.xml"))
  :hook
  ;;; disable display-line-numbers-mode in elfeed-show-mode
  (elfeed-show-mode . (lambda () (display-line-numbers-mode -1))))

(use-package elfeed-webkit
  :ensure
  ;; :demand ;; !
  :after elfeed
  :init
  (setq elfeed-webkit-auto-enable-tags '(webkit comics))
  :config
  (elfeed-webkit-auto-toggle-by-tag)
  :bind (:map elfeed-show-mode-map
              ("%" . elfeed-webkit-toggle)))

(provide 'init-rss)

;; init-rss.el ends here
