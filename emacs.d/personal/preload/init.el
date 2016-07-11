;;; package --- Summary

;;; Commentary:

;;; Code:

;;Proxy
;(setq url-proxy-services
;   '(("no_proxy" . "^\\(localhost\\|10.*\\)")
;     ("http" . "proxy-url:8080")
;     ("https" . "proxy-url:8080")))

;(setq url-http-proxy-basic-auth-storage
;    (list (list "proxy-url:8080"
;                (cons "Input your LDAP UID !"
;                      (base64-encode-string "antonova:XXXXXXX")))))

;; Disabling default prelude theme
(setq prelude-theme nil)

;; Adding additional repos to package manager
(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

(provide 'init)
;;; init.el ends here
