;; Generating This Document

;; Here's a convenience function that generates the files mentioned in
;; the previous section:


(defun export-init-org ()
  "Generate init.el and init.htm from the current init.org file."
  (interactive)
  (call-interactively #'org-babel-tangle)
  (call-interactively #'org-html-export-as-html))

;; General Folder Structure

;;  The load path is the list of directories that Emacs searches for
;;  executing Elisp code (i.e. by using "require").

;;  I store the Emacs Lisp code that I write inside the user-lisp folder.


(add-to-list 'load-path "~/.emacs.d/user-lisp")

;; Package Management

;; I use ELPA packages, mainly from the MELPA repository:


(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))
(package-initialize)



;; Bootstrap use-package:


(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))



;; Add an environment variable required by the PDF Tools package:


(setenv "PKG_CONFIG_PATH" "/usr/local/Cellar/zlib/1.2.8/lib/pkgconfig:/usr/local/lib/pkgconfig:/opt/X11/lib/pkgconfig")

;; General Customizations

;; Start Emacs fully maximized:


(add-to-list 'default-frame-alist '(fullscreen . maximized))



;; Some general functions that I've written.


(require 'defuns-config)



;; And some general keybinding modifications.


(require 'keybindings)

;; Mac

;; On a Mac, I use the Command key as Meta.


(setq mac-option-modifier 'none)
(setq mac-command-modifier 'meta)
(setq ns-function-modifier 'hyper)



;; GUI apps on macOS do not inherit $PATH from shell. This package solves
;; that.


(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns))
  :config
  (progn
    (setq exec-path-from-shell-arguments '("-l"))
    (setq exec-path-from-shell-variables
          (append exec-path-from-shell-variables '("JAVA_HOME")))
    (exec-path-from-shell-initialize)))

;; Appearance and Themes

;; I use a custom black theme by default, but I open the possibility to
;; use another for a special purpose (excellent Org-Mode support, a
;; presentation, etc.)


(require 'appearance)

;; Ediff

;; I like Ediff's control panel to show in a separate frame, even on
;; graphical environments.


(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; File Backup Management

;; Store Emacs backup files in their own directory, so as not to pollute
;; the current directory.


(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-list-file-name-transforms '((".*", "~/.emacs.d/auto-save-list" t)))

;; Ido and Recentf

;; I use Ido ("Interactive Do") as a simple way to switch between open
;; buffers.


(ido-mode t)



;; Recentf is a mode for displaying recently open files.


(recentf-mode 1)

;; Line Numbers

;; I use linum mode, but only for programming modes.


(add-hook 'prog-mode-hook 'linum-mode)



;; Format linum-mode output by appending a space at the end.


(setq linum-format "%d ")



;; I also highlight the current line, only for programming modes.


(add-hook 'prog-mode-hook 'hl-line-mode)

;; Mode Line

;; Show in which function or method the point is.


(which-function-mode 1)



;; Show column numbers in the mode line.


(column-number-mode 1)



;; I don't want some minor modes polluting my mode line. I use the
;; diminish package to avoid that.


(use-package diminish
  :ensure t
  :defer t)



;; Show the current time.


(use-package time
  :config
  (progn
    (setf display-time-default-load-average nil
          display-time-use-mail-icon t
          display-time-24hr-format nil)
    (display-time-mode t)))

;; Navigation Tree

;; Show a project navigation tree using Neotree package.


(use-package neotree
  :ensure t
  :defer t
  :config
  (global-set-key [f8] 'neotree-toggle))

;; Pairs

;; To ease working with pairs, I use the smartparens package.


(use-package smartparens
  :ensure t
  :defer t)

(use-package smartparens-config
  :ensure smartparens
  :defer t
  :config
  (progn
    (show-smartparens-global-mode t))
    (add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
    (add-hook 'markdown-mode-hook 'turn-on-smartparens-strict-mode))



;; Highlight parentheses pairs.


(show-paren-mode 1)
(set-face-background 'show-paren-match-face "#aaaaaa")
(set-face-attribute 'show-paren-match-face nil
:weight 'bold :underline nil :overline nil :slant
'normal)



;; Close pairs automatically.


(electric-pair-mode 1)



;; Rainbow-delimiters is a package which highlights delimiters such as
;; parentheses, brackets or braces according to their depth


(use-package rainbow-delimiters
  :ensure t
  :defer t
  :init
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'ielm-mode-hook #'rainbow-delimiters-mode)
  :config
  (set-face-foreground 'rainbow-delimiters-depth-1-face "snow4")
  (setf rainbow-delimiters-max-face-count 1)
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                      :foreground 'unspecified
                      :inherit 'error)
  (set-face-foreground 'rainbow-delimiters-depth-1-face "snow4"))

;; Trailing Whitespace

;; Remove trailing whitespace before saving a file.


(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Window Management

;; I use winner-mode to manage my windows with convenient undo/redo functions.


(winner-mode 1)

;; Programming Language Customizations

;; These are my customizations for the programming languages I use most.

;; I generally dislike tabs in my programs.


(setq-default indent-tabs-mode nil)

;; C/C++/Objective-C/Objective-C++

;; For C languages, I use K&R style, with an indentation of 2 spaces.


(use-package cc-mode
  :config
  (add-hook 'c-mode-hook (lambda ()
                           (c-set-style "k&r")
                           (setq c-basic-offset 2))))



;; As there's not a specific Emacs mode for this programming language,
;; for Objective-C++ files, use Objective-C mode.


(add-to-list 'auto-mode-alist '("\\.mm$" . objc-mode))



;; Use a modern font lock mechanism.


(use-package modern-cpp-font-lock
  :ensure t
  :defer t
  :config
  (add-hook 'c++-mode-hook #'modern-c++-font-lock-mode))



;; Use LSP with flycheck, company, and cquery as C++ client.


(use-package lsp-mode
  :ensure t
  :config
  (require 'lsp-flycheck))

(use-package company-lsp
  :ensure t
  :config
  (add-to-list 'company-backends 'company-lsp))

(add-to-list 'load-path "~/Projects/cquery/emacs/")
(require 'cquery)
(setq cquery-executable (expand-file-name "~/Projects/cquery/build/app"))



;; I use YouCompleteMe for C/C++ semantic autocompletion.


(use-package ycmd
  :disabled
  :ensure t
  :defer t
  :config
  (add-hook 'c++-mode-hook 'ycmd-mode)
  (set-variable 'ycmd-server-command (list "python" (substitute-in-file-name "$HOME/.emacs.d/vendor/ycmd/ycmd/__main__.py")))
  (setq ycmd-parse-conditions '(save new-line mode-enabled idle-change))
  (setq-default ycmd-request-log-level 'verbose)
  (setq-default ycmd-request-message-level 'verbose)
  (set-variable 'ycmd-extra-conf-whitelist '("~/Projects/*"))
  (setq url-show-status nil))



;; I integrate ycm with company.


(use-package company-ycmd
  :disabled
  :ensure t
  :defer t
  :init
  (company-ycmd-setup)
  :config
  (setq company-idle-delay 0.1)
  (add-hook 'c++-mode-hook 'company-mode))



;; And also with flycheck.


(use-package flycheck-ycmd
  :disabled
  :ensure t
  :defer t
  :init
  (add-hook 'ycmd-mode-hook 'flycheck-ycmd-setup)
  :config
  (add-hook 'c++-mode-hook 'flycheck-mode))

;; Clojure

;; Cider is the "de facto" package for working on Clojure projects.


(use-package cider
  :ensure t
  :defer t)

;; Emacs Lisp

;; Suggest.el is a nice package that helps you discover Elisp functions
;; that do what you want.


(use-package suggest
  :ensure t
  :defer t)

;; Haskell

;; For Haskell I use haskell-mode.


(use-package haskell-mode
  :ensure t)

;; JavaScript

;; Use js2-mode for JavaScript.


(use-package js2-mode
  :ensure t
  :mode ("\\.js\\'" . js2-mode))

;; Kotlin

;; Use kotlin-mode for Kotlin development.


(use-package kotlin-mode
  :ensure t
  :mode ("\\.kt\\'" . kotlin-mode))

;; LaTeX

;; Use Auctex with tex-site for an excellent LaTeX environment. Also,
;; enable RefTeX mode whenever a LaTeX document is open.


(use-package tex-site
  :ensure auctex
  :defer t
  :config
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex))

;; Markdown

;; I use markdown-mode to work on Markdown (.md) documents.


(use-package markdown-mode
  :ensure t
  :mode ("\\.\\(m\\(ark\\)?down\\|md\\)$" . markdown-mode))



;; I want to fontify code blocks in Markdown:


(setq markdown-fontify-code-blocks-natively t)

;; PHP

;; Emacs does not come with a mode for editing PHP mode. Just use
;; php-mode form the package repository.


(use-package php-mode
  :ensure t
  :mode ("\\.php\\'" . php-mode))

;; Python

;; There are several packages for writing Python code. I use python.


(use-package python
  :ensure t
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode))



;; Anaconda provides navigation documentation lookup and code completion
;; for Python:


(use-package anaconda-mode
  :ensure t
  :after python
  :config
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode))



;; Integrate Anaconda with company:


(use-package company-anaconda
  :ensure t
  :after company
  :config
  (add-to-list 'company-backends 'company-anaconda))



;; Format Python code according to PEP8:


(use-package py-autopep8
  :ensure t
  :bind
  (:map python-mode-map
        ("C-c u" . py-autopep8-buffer))
  :config
  (setq py-autopep8-options '("--max-line-length=79")))

;; Shell

;; For linting Shell scripts, I integrate Shellcheck with Flycheck.


(add-hook 'sh-mode-hook 'flycheck-mode)

;; Swift

;; I use swift-mode for Swift code.


(use-package swift-mode
  :ensure t
  :mode ("\\.swift\\'" . swift-mode)
  :config
  (setq flycheck-swift-sdk-path "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.11.sdk")
  (add-to-list 'flycheck-checkers 'swift))

;; TableGen

;; TableGen is an abstract IDL used by LLVM and related projects to
;; generate code automatically.


(add-to-list 'load-path "~/Projects/llvm/utils/emacs")
(require 'tablegen-mode)

;; Autocompletion

;; Autocompletion is very important for programming languages and natural
;; languages. I use company for that.


(use-package company
  :ensure t
  :defer t
  :diminish company-mode
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-backends (delete 'company-semantic company-backends)))

;; CMake

;; CMake is a meta-build system that is commonly used in C++ projects.


(use-package cmake-mode
  :ensure t
  :defer t)

;; Code Formatting

;; Code formatting tools make smarter decisions than typical Emacs
;; indenters, specially for complex languages like C++. As yet, I use
;; clang-format for C++ and related languages.


(use-package clang-format
  :ensure t
  :bind
  (:map c++-mode-map
        ("C-c i" . clang-format-region)
        ("C-c u" . clang-format-buffer)))

;; Code Navigation

;; Sourcetrail is a great indexer to make sense of a big C/C++/Java
;; project.


(use-package sourcetrail
  :ensure t
  :bind
  ("C-c s" . sourcetrail-send-location))



;; For quick navigation inside a source file, I use ace-jump-mode.


(use-package ace-jump-mode
  :ensure t
  :defer t
  :init
  (global-set-key (kbd "C-c SPC") 'ace-jump-mode))



;; Typically, I want to navigate quickly over the instances of a
;; particular symbol in a source file.


(use-package highlight-symbol
  :ensure t
  :config
  (define-key prog-mode-map (kbd "M-n") 'highlight-symbol-next)
  (define-key prog-mode-map (kbd "M-p") 'highlight-symbol-prev)
  ;; Modes that inherit from c-mode aren't affected by prog-mode-map,
  ;; so we have to set bindings again.
  (define-key c-mode-map (kbd "M-n") 'highlight-symbol-next)
  (define-key c-mode-map (kbd "M-p") 'highlight-symbol-prev)
  (define-key c++-mode-map (kbd "M-n") 'highlight-symbol-next)
  (define-key c++-mode-map (kbd "M-p") 'highlight-symbol-prev)
  (define-key java-mode-map (kbd "M-n") 'highlight-symbol-next)
  (define-key java-mode-map (kbd "M-p") 'highlight-symbol-prev))

;; Code Selection

;; Use expand-region to increase the selected region by semantic units.


(use-package expand-region
  :ensure t
  :defer t
  :init
  (global-set-key (kbd "C-=") 'er/expand-region))

;; Debugging

;; Debugging is very important when working on a program. I use RealGud,
;; which is a nice abstraction over several debuggers for programming
;; languages.


(use-package realgud
  :ensure t
  :defer t)

;; Documentation

;; For showing inline documentation for Emacs Lisp functions, I use eldoc.


(use-package eldoc
  :ensure t
  :defer t
  :diminish eldoc-mode
  :config
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode))



;; In general, I use Dash docsets for any programming language. For now,
;; dash-at-point only works for C++ files.


(use-package dash-at-point
  :ensure t
  :config
  (add-to-list 'dash-at-point-mode-alist '(c++-mode . "cpp"))
  :bind
  ("C-c h" . dash-at-point))

;; Magit

;; Magit is the best Git porcelain I've ever used.


(use-package magit
  :ensure t
  :bind
  ("C-x g" . magit-status)
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

;; MagitHub

;; MagitHub extends Magit with functions to work on GitHub repositories
;; (show open issues, PRs, etc.).


(use-package magithub
  :after magit
  :ensure t
  :config
  (magithub-feature-autoinject t))

;; Git TimeMachine

;; git-timemachine is a package that intuitively shows previous versions
;; of a particular file from a Git repository.


(use-package git-timemachine
  :ensure t
  :defer t)

;; Git Undo


;; Git-undo lets you select a region and revert changes in that region to
;; the most recent Git historical version.


(add-to-list 'load-path "~/.emacs.d/user-lisp/git-undo")

;; Browse at Remote

;; This package browses target pages at GitHub/Bitbucket.


(use-package browse-at-remote
  :ensure t
  :bind
  ("C-c g g" . browse-at-remote))

;; Helm

;; Helm is a great incremental completion and selection narrowing framework.


(use-package helm
  :ensure t
  :diminish helm-mode
  :init (helm-mode)
  :config
  (setq helm-ff-auto-update-initial-value t)
  :bind
  (;; Redefine M-x to use Helm
   ("M-x" . helm-M-x)
   ;; File navigation on steroids
   ("C-x C-f" . helm-find-files)
   ;; Great kill ring cycling
   ("M-y" . helm-show-kill-ring)
   ;; Find buffers and recent files using Helm mini
   ("C-x b" . helm-mini)
   :map helm-map
   ("<tab>" . helm-execute-persistent-action)
   ("C-i" . helm-execute-persistent-action)
   ("C-z" . helm-select-action)))

;; Helpful

;; Better help system.


(use-package helpful
  :ensure t
  :defer t)

;; Natural Languages

;; For checking spelling and grammar, I use an external Java tool: Language-tool.


(use-package langtool
  :ensure t
  :defer t
  :config
  (setq langtool-language-tool-jar "/usr/local/Cellar/languagetool/3.7/libexec/languagetool-commandline.jar"))

;; Org-Mode

;; Org-Mode configuration is handled in a separate file.


(require 'org-mode-config)

;; PDF Tools

;; I want a nice way to work on PDF documents graphically.

;; Install with `brew install pdf-tools`.


(use-package pdf-tools
  :ensure t
  :config
  (custom-set-variables
   '(pdf-tools-handle-upgrades nil))
  (setq pdf-info-epdfinfo-program "/usr/local/bin/epdfinfo")
  (pdf-tools-install))



;; pdf-linter will "lint" a PDF document using PDFBox Preflight app.


(require 'pdf-linter)
(setq pdf-linter-jar "$HOME/PDFBox/preflight-app-2.0.7.jar")



;; Interleave is a minor mode to interleave notes in PDF books/papers.


(use-package interleave
  :ensure t
  :after pdf-tools)

;; Project Management

;; Programs are usually organized in projects, being a Git repo a natural
;; way to define one. I use Projectile to work on projects.


(use-package projectile
  :ensure t
  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'helm)
  (helm-projectile-on))



;; I also integrate Projectile with Helm.


(use-package helm-projectile
  :ensure t
  :after projectile)

;; Pandoc

;; Pandoc is a tool to convert between almost every document format.


(use-package pandoc-mode
  :ensure t
  :defer t)

;; REST

;; For making REST calls from Emacs, I use the convenient restclient package.


(use-package restclient
  :ensure t
  :defer t)

;; Search

;; I like fast searches of text and symbols inside a project. As a faster
;; way of grepping a project, I use Silver Searcher (ag) and integrate it
;; with Helm.


(use-package helm-ag
  :ensure t
  :bind ("C-c p A" . helm-do-ag-project-root))

;; Snippets and Abbreviations

;; I use yasnippet for managing text snippets.


(use-package yasnippet
  :ensure t
  :defer t
  :diminish yas-minor-mode
  :init (yas-global-mode 1))



;; Manage abbreviations with abbrev-mode.


(use-package abbrev
  :diminish abbrev-mode)

;; Syntax checking

;; I use flycheck for "on the fly" syntax checking.


(use-package flycheck
  :ensure t
  :defer t
  :init (global-flycheck-mode))



;; Don't check documentation by default (good thing for small throwaway scripts).


(setq flycheck-checkers (--remove (eq it 'emacs-lisp-checkdoc) flycheck-checkers))

;; Undo

;; For a more intuitive undo/redo management, I use undo-tree instead of
;; the default undo/redo system.


(use-package undo-tree
  :ensure t
  :defer t
  :diminish undo-tree-mode
  :init (global-undo-tree-mode)
  :config
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t))

;; Wordpress

;; For editing a Wordpress blog, use org2blog.


(require 'org2blog-config)

;; X.509

;; I've created a simple major mode that toggles between showing raw and
;; detailed information about a X.509 certificate.


(require 'x509-certificate-mode)

;; Xcode Projects

;; I've created a package for working on Xcode projects.


(add-to-list 'load-path "~/.emacs.d/user-lisp/pbxproj-mode")
(require 'pbxproj-mode)



;; I've also added on-the-fly syntax checking capabilities.


(add-to-list 'load-path "~/.emacs.d/user-lisp/flycheck-pbxproj")
(require 'flycheck-pbxproj)
(flycheck-pbxproj-setup)
