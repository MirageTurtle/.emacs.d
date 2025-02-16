;; init-org-publish.el -*- lexical-binding: t; -*-

;;; set some path
(setq mt/project-dir (expand-file-name "~/Documents/personal_projects")) ;; set my project directory
(setq mt/org-website-dir (concat mt/project-dir "/org-website")) ;; set my org-website directory
(setq org-publish-timestamp-directory (concat mt/org-website-dir "/.org-timestamps/")) ;; set the timestamp directory
(setq mt/org-website-static-dir (concat mt/org-website-dir "/res")) ;; set the static directory
(setq mt/org-website-publish-dir (concat mt/org-website-dir "/public")) ;; set the publish directory

;;; set extra head, header and footer
(setq mt/extra-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"/res/style.css\">") ;; extra css
(setq mt/header-file (concat mt/org-website-static-dir "/header.html"))
(setq mt/footer-file (concat mt/org-website-static-dir "/footer.html"))
(defun mt/header (arg)
  (with-temp-buffer
    (insert-file-contents mt/header-file)
    (buffer-string)))
(defun mt/footer (arg)
  (with-temp-buffer
    (insert-file-contents mt/footer-file)
    (buffer-string)))

;;; some extra settings
(setq org-html-html5-fancy t)

;;; util functions
;; (defun mt/blog-minify-css ()
;;   (let* ((csstidy "csstidy")
;;          (csstidy-args " --template=highest --silent=true")
;;          (css-dir (expand-file-name (plist-get project-plist :publishing-directory)))
;;          (css-files (directory-files css-dir t "^.*\\.css$")))
;;     (dolist (file css-files)
;;       (with-temp-buffer
;;         (insert (shell-command-to-string (concat csstidy " " file csstidy-args)))
;;         (write-file file)))))

;;; set org-publish-project-alist
(setq org-publish-project-alist
      `(("org-website"
	 :components ("org-website-index" "org-website-posts" "org-website-res"))
	("org-website-index"
	 :base-directory ,mt/org-website-dir
	 :base-extension "org"
	 :publishing-directory ,mt/org-website-publish-dir
      	 :publishing-function org-html-publish-to-html
	 :recursive t
	 :htmlized-source t ;; this enables htmlize, which means that I can use css for code!
	 :index-filename "index.org"
	 
	 :with-author t
	 :with-creator nil
	 :with-date t
	 :with-toc nil
	 :with-tags nil

	 :section-numbers nil
	 
	 :html-doctype "html5"
	 :html-link-home "/" ;;
	 :html-head nil ;; cleans up anything that would have been in there.
	 :html-head-extra ,mt/extra-head
	 :html-preamble mt/header
	 :html-postamble mt/footer
	 :html-head-include-default-style nil
         :html-head-include-scripts nil
         :html-viewport nil
	 :html-home/up-format "" ;; remove the home/up link
	 )
	("org-website-posts"
	 :base-directory ,(concat mt/org-website-dir "posts")
	 :base-extension "org"
	 :publishing-directory ,(concat mt/org-website-publish-dir "posts")
      	 :publishing-function org-html-publish-to-html
	 :recursive t
	 :htmlized-source t ;; this enables htmlize, which means that I can use css for code!
	 
	 :with-author t
	 :with-creator nil
	 :with-date t
	 :with-tags nil

	 :section-numbers nil
	 :with-toc nil

	 :html-doctype "html5"
	 :html-link-home "/" ;;
	 :html-head nil ;; cleans up anything that would have been in there.
	 :html-head-extra ,mt/extra-head
	 :html-preamble mt/header
	 :html-postamble mt/footer
	 :html-head-include-default-style nil
         :html-head-include-scripts nil
         :html-viewport nil
	 :html-home/up-format "" ;; remove the home/up link
	 
	 ;; ;; sitemap
	 ;; :auto-sitemap t
	 ;; :sitemap-filename "index.org"
	 ;; :sitemap-title "MirageTurtle's Tree House"
	 ;; :sitemap-sort-files anti-chronologically
	 ;; :sitemap-file-entry-format "%d %t"
	 ;; :sitemap-date-format "%Y-%m-%d"
	 ;; :sitemap-style list
	 )
	;;; static files
	("org-website-res"
         :base-directory ,mt/org-website-static-dir
         :base-extension ".*"
         :publishing-directory ,(concat mt/org-website-publish-dir "/res")
         :publishing-function org-publish-attachment)))


;;; publish
;;; rsync to server
(setq mt/remote-host "debian")
(setq mt/org-website-remote-dir "~/documents/org")
(defun mt/rsync-org-website ()
  (interactive)
  (let ((cmd (format "rsync -avz --delete --update %s/ %s:%s"
		     mt/org-website-publish-dir
		     mt/remote-host
		     mt/org-website-remote-dir)))
    (message "Running command: %s" cmd)
    (shell-command cmd)))

(provide 'init-org-publish)

;;; init-org-publish.el ends here
