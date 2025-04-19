;; init-net.el -*- lexical-binding: t; -*-
;; For the network related settings

;; some variables
(setq mt/no_proxy_regex "^\\(localhost\\|10\\..*\\|192\\.168\\..*\\)")
;; Define the alist for potential proxy names
(setq mt/proxy-alist '(
		       ("clash" . "http://127.0.0.1:7897")
		       ("clash-socks5" . "socks5://127.0.0.1:7897")))

;; some functions for proxy
;;; clash
(defun mt/set-proxy (proxy-name)
  "Set the proxy for Emacs and curl by proxy NAME."
  (interactive
   (list (completing-read "Choose proxy: " (mapcar 'car mt/proxy-alist))))
  (let ((proxy (cdr (assoc proxy-name mt/proxy-alist))))
    (if proxy
        (progn
          (mt/set-emacs-proxy proxy)
          (mt/set-curl-proxy proxy))
      (message "Proxy not found!"))))

(defun mt/unset-proxy ()
  "Unset the proxy for Emacs and curl."
  (interactive)
  (mt/unset-emacs-proxy)
  (mt/unset-curl-proxy)
  (message "Proxy has been unset."))

(defun mt/set-emacs-proxy (proxy)
  "Set the proxy for Emacs to use the given PROXY."
  (setq url-proxy-services `(("no_proxy" . ,mt/no_proxy_regex)
                             ("http" . ,proxy)
                             ("https" . ,proxy))))

(defun mt/set-curl-proxy (proxy)
  "Set the proxy for curl to use the given PROXY."
  (setenv "http_proxy" proxy)
  (setenv "https_proxy" proxy))

(defun mt/get-scheme-host-port-from-proxy (proxy)
  "Get the scheme, host and port from the given PROXY."
  (let* ((proxy-split (split-string proxy "://"))
	 (scheme (car proxy-split))
	 (host-port (cadr proxy-split))
	 (host-port-split (split-string host-port ":"))
	 (host (car host-port-split))
	 (port (string-to-number (cadr host-port-split))))
    (list scheme host port)))

(defun mt/set-telega-proxy-helper (proxy)
  "Set the proxy for telega to use the given PROXY."
  (if (featurep 'telega)
      (let* ((proxy-scheme-host-port (mt/get-scheme-host-port-from-proxy proxy))
             (proxy-scheme (car proxy-scheme-host-port))
             (proxy-host (cadr proxy-scheme-host-port))
             (proxy-port (caddr proxy-scheme-host-port))
             (type (cond
                    ((string= proxy-scheme "socks5")
                     '(:@type "proxyTypeSocks5"))
                    ((string= proxy-scheme "http")
                     '(:@type "proxyTypeHttp"))
                    ((string= proxy-scheme "mtproto")
                     '(:@type "proxyTypeMtproto"))))
             (proxy `(:server ,proxy-host :port ,proxy-port :enable t :type ,type)))
        (setq telega-proxies (list proxy))
        (message "telega proxy set to %s" proxy))
    (message "telega not loaded!")))


(defun mt/set-telega-proxy (proxy-name)
  "Set the proxy for telega by proxy NAME."
  (interactive
   (list (completing-read "Choose proxy: " (mapcar 'car mt/proxy-alist))))
  (let ((proxy (cdr (assoc proxy-name mt/proxy-alist))))
    (if proxy
	(mt/set-telega-proxy-helper proxy)
      (message "Proxy not found!"))))

;;; unset proxy
(defun mt/unset-emacs-proxy ()
  "Unset the proxy for Emacs."
  (setq url-proxy-services nil))
(defun mt/unset-curl-proxy ()
  "Unset the proxy for curl."
  (setenv "http_proxy" nil)
  (setenv "https_proxy" nil))

;; (mt/set-proxy "clash")

(provide 'init-network)

;;; init-net.el ends here.
