;; Emacs configuration file

;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGE MANAGEMENT ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;;;;;;;;;;;;
;; GENERAL ;;
;;;;;;;;;;;;;

;; Set load-path
(let ((default-directory  "~/.emacs.d/"))
  (normal-top-level-add-subdirs-to-load-path))

;; Set some custom folders where binaries are located
(setq exec-path (append '("/usr/local/bin") exec-path))
(setenv "PATH" (concat "/usr/texbin" ":" (concat "/usr/local/bin" ":" (getenv "PATH"))))

;; Configure some appearance options
(require 'appearance)

;; Use the command key as meta key
(setq mac-option-modifier 'none)
(setq mac-command-modifier 'meta)
(setq ns-function-modifier 'hyper)

;; Show in which function/method the point is
(which-function-mode 1)

;; Highlight parentheses pairs
(show-paren-mode 1)
(set-face-background 'show-paren-match-face "#aaaaaa")
(set-face-attribute 'show-paren-match-face nil
        :weight 'bold :underline nil :overline nil :slant
        'normal)

;; Close parenthesis, braces, etc. automatically
(electric-pair-mode 1)

;; Show column numbers in status bar
(column-number-mode 1)

;; Format linum-mode output by appending a space at the end
(setq linum-format "%d ")

;; ido-mode
(ido-mode t)

;; recent mode
(recentf-mode 1)

;; Scroll pages up and down
(global-set-key "\M-p" (lambda () (interactive) (scroll-down 4)))
(global-set-key "\M-n" (lambda () (interactive) (scroll-up 4)))

;; File backup management
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-list-file-name-transforms '((".*", "~/.emacs.d/auto-save-list" t)))

;; Winner mode
(winner-mode 1)

;; Linum mode
(add-hook 'prog-mode-hook (lambda () (linum-mode 1)))

;; Function definitions
(require 'defuns-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PROGRAMMING LANGUAGES ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; General
(setq-default indent-tabs-mode nil) ;; No tabs, please

;; C/C++
(use-package cc-mode
  :config
  ;; Use K&R style for C
  (add-hook 'c-mode-hook (lambda ()
                           (c-set-style "k&r")
                           ;; Indent with 4 spaces
                           (setq c-basic-offset 4)
                           (setq tab-width 4))))
;; JavaScript
(use-package js2-mode
  :ensure t
  :mode ("\\.js\\'" . js2-mode))

;; Octave
(use-package octave
  :ensure t
  :mode ("\\.m$" . octave-mode))

;; LaTeX
(use-package tex-site
  :ensure auctex
  :config
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex))

;; Python
(use-package python
  :ensure t
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode))

;; Markdown
(use-package markdown-mode
  :ensure t
  :mode ("\\.\\(m\\(ark\\)?down\\|md\\)$" . markdown-mode))

;; PHP
(use-package php-mode
  :ensure t
  :mode ("\\.php\\'" . php-mode))

;; Swift
(use-package swift-mode
  :ensure t
  :mode ("\\.swift\\'" . swift-mode))

;;;;;;;;;;;;;;;;;;;
;; GENERAL TOOLS ;;
;;;;;;;;;;;;;;;;;;;

;; Magit
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

(use-package gh
  :ensure t)

(use-package s
  :ensure t)

(use-package magit-gh-pulls
  :load-path "vendor/magit-gh-pulls/"
  :config
  (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls))

(use-package git-timemachine
  :ensure t)

;; Helm
(use-package helm
  :ensure t
  :init (helm-mode)
  :bind
  (;; Redefine M-x to use Helm
   ("M-x" . helm-M-x)
   ;; File navigation on steroids
   ("C-x C-f" . helm-find-files)
   ;; Greate kill ring cycling
   ("C-y" . helm-show-kill-ring)
   ;; Find recent files using Helm
   ("C-c f" . helm-recentf)
   :map helm-map
   ("<tab>" . helm-execute-persistent-action)
   ("C-i" . helm-execute-persistent-action)
   ("C-z" . helm-select-action)))

;; Yasnippet
(use-package yasnippet
  :ensure t
  :init (yas-global-mode 1))

;; Restclient
(use-package restclient
  :ensure t)

;; Undo Tree
(use-package undo-tree
  :ensure t
  :init (global-undo-tree-mode)
  :config
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t))

;; Company
(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-backends (delete 'company-semantic company-backends)))

;; ELDoc
(use-package eldoc
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode))

;; Smartparens
(use-package smartparens
  :ensure t
  :config
  (require 'smartparens-config)
  (show-smartparens-global-mode t)
  (add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
  (add-hook 'markdown-mode-hook 'turn-on-smartparens-strict-mode))

;; Projectile
(use-package helm-projectile
  :ensure t)

(use-package projectile
  :ensure t
  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'helm)
  (helm-projectile-on))

;; Silver Searcher
(use-package helm-ag
  :ensure t
  :bind ("C-c a g" . helm-do-ag-project-root))

;; Irony Mode
(use-package irony
  :ensure t
  :config
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  (defun my-irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async))
  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

;; Company Irony
(use-package company-irony
  :ensure t
  :config
  (eval-after-load 'company
    '(add-to-list
      'company-backends 'company-irony)))

;; Clang-Format
(use-package clang-format
  :ensure t
  :bind
  ("C-c i" . clang-format-region)
  ("C-c u" . clang-format-buffer))

;; Suggest
(use-package suggest
  :ensure t)

;; RTags
(use-package rtags
  :ensure t
  :config
  (require 'company-rtags)
  (use-package company
    :config
    (add-to-list 'company-backends 'company-rtags))
  (setq rtags-completion-enabled t)
  (setq rtags-autostart-diagnostics t)
  (rtags-enable-standard-keybindings)
  (setq rtags-use-helm t))

;; Flycheck
(defun my-flycheck-rtags-setup ()
  (flycheck-select-checker 'rtags)
  (setq-local flycheck-highlighting-mode nil)
  (setq-local flycheck-check-syntax-automatically nil))

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode)
  :config
  (require 'flycheck-rtags)
  (add-hook 'c-mode-common-hook #'my-flycheck-rtags-setup))

(use-package flycheck-irony
  :ensure t
  :config
  (eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup)))

;; Clojure
(use-package cider
  :ensure t)

;;;;;;;;;;;;;;;;;
;; KEYBINDINGS ;;
;;;;;;;;;;;;;;;;;

(require 'keybindings)

;;; init.el ends here

