;;; org2blog-config.el --- Org-Mode to Wordpress configuration.

;;; Commentary:

;;; Code:
(use-package org2blog
  :ensure t
  :defer t
  :config
  (require 'org2blog-autoloads)
  (setq org2blog/wp-confirm-post t)
  (setq org2blog/wp-blog-alist
      `(("cocoaengineering"
         :url "https://cocoaengineering.wordpress.com/xmlrpc.php"
         :username ,(car (auth-source-user-and-password "cocoaengineering.wordpress.com"))
         :password ,(cadr (auth-source-user-and-password "cocoaengineering.wordpress.com"))))))

(provide 'org2blog-config)
