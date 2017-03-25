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
(setenv "PATH" (concat "/Library/TeX/texbin" ":" (concat "/usr/local/bin" ":" (getenv "PATH"))))
(setenv "PKG_CONFIG_PATH" "/usr/local/Cellar/zlib/1.2.8/lib/pkgconfig:/usr/local/lib/pkgconfig:/opt/X11/lib/pkgconfig")

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
(add-hook 'prog-mode-hook 'linum-mode)

;; Highlight current line in programming modes
(add-hook 'prog-mode-hook 'hl-line-mode)

;; Don't show ediff's control panel in a separate frame, even on graphical environments
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

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

;; Use Obj-C mode for Obj-C++ files
(add-to-list 'auto-mode-alist '("\\.mm$" . objc-mode))

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
  :load-path "vendor/swift-mode/"
  :mode ("\\.swift\\'" . swift-mode)
  :config
  (setq flycheck-swift-sdk-path "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.11.sdk")
  (add-to-list 'flycheck-checkers 'swift))

;; Clojure
(use-package cider
  :ensure t)

;;;;;;;;;;;;;;;;;;;
;; GENERAL TOOLS ;;
;;;;;;;;;;;;;;;;;;;

;; Magit
(use-package magit
  :ensure t
  :bind
  ("C-x g" . magit-status)
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

(use-package gh
  :ensure t)

(use-package s
  :ensure t)

(use-package magithub
  :after magit
  :config
  (magithub-feature-autoinject t))

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
   ;; Great kill ring cycling
   ("M-y" . helm-show-kill-ring)
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
  :ensure t)

(use-package smartparens-config
  :ensure smartparens
  :config
  (progn
    (show-smartparens-global-mode t))
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

;; Clang-Format
(use-package clang-format
  :ensure t
  :bind
  ("C-c i" . clang-format-region)
  ("C-c u" . clang-format-buffer))

;; Suggest
(use-package suggest
  :ensure t)

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode))

(use-package ox-reveal
  :ensure t)

;; YouCompleteMe
(use-package ycmd
  :ensure t
  :config
  (add-hook 'c++-mode-hook 'ycmd-mode)
  (set-variable 'ycmd-server-command (list "python" (substitute-in-file-name "$HOME/ycmd/ycmd/__main__.py")))
  (setq ycmd-parse-conditions '(save new-line mode-enabled idle-change))
  (setq url-show-status nil))

(use-package company-ycmd
  :ensure t
  :init
  (company-ycmd-setup)
  :config
  (setq company-idle-delay 0.1)
  (add-hook 'c++-mode-hook 'company-mode))

(use-package flycheck-ycmd
  :ensure t
  :init
  (flycheck-ycmd-setup)
  :config
  (add-hook 'c++-mode-hook 'flycheck-mode))

;; Ace-jump-mode
(use-package ace-jump-mode
  :ensure ace-jump-mode
  :init
  (global-set-key (kbd "C-c SPC") 'ace-jump-mode))

;; Rtags
(use-package rtags
  :ensure t
  :config
  (setq rtags-autostart-diagnostics t)
  (setq rtags-use-helm t)
  (add-hook 'c-mode-hook 'rtags-start-process-unless-running)
  (add-hook 'c++-mode-hook 'rtags-start-process-unless-running)
  (define-key c-mode-base-map (kbd "M-.") 'rtags-find-symbol-at-point)
  (define-key c-mode-base-map (kbd "M-,") 'rtags-find-references-at-point)
  (define-key c-mode-base-map (kbd "M-]") 'rtags-location-stack-back)
  (define-key c-mode-base-map (kbd "M-[") 'rtags-location-stack-forward))

;; Dash (DocSets)
(use-package dash-at-point
  :ensure t
  :config
  (add-to-list 'dash-at-point-mode-alist '(c++-mode . "cpp"))
  :bind
  ("C-c h" . dash-at-point))

;; RealGud
(use-package realgud
  :ensure t)

;; CMake mode
(use-package cmake-mode
  :ensure t)

;; PDF Tools
(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install))

;;;;;;;;;;;;;;;;;
;; KEYBINDINGS ;;
;;;;;;;;;;;;;;;;;

(require 'keybindings)

;;; init.el ends here

