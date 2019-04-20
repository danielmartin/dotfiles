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

;; Load package.el:


(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa")))
(package-initialize)



;; Bootstrap use-package:


(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))



;; Add an environment variable required by the PDF Tools package:


(setenv "PKG_CONFIG_PATH" "/usr/local/Cellar/zlib/1.2.8/lib/pkgconfig:/usr/local/lib/pkgconfig:/opt/X11/lib/pkgconfig")

;; General Customizations

;; Make sure we can debug init errors more easily:


(if init-file-debug
    (setq use-package-verbose t
          use-package-expand-minimally nil
          use-package-compute-statistics t
          debug-on-error t)
  (setq use-package-verbose nil
        use-package-expand-minimally t))



;; Start Emacs fully maximized:


(add-to-list 'default-frame-alist '(fullscreen . maximized))



;; Some general functions that I've written.


(require 'defuns-config)



;; And some general keybinding modifications.


(require 'keybindings)

;; Custom Keymaps

;; Define my custom prefix keys:


(eval-and-compile
  (mapc #'(lambda (entry)
            (define-prefix-command (cdr entry))
            (bind-key (car entry) (cdr entry)))
        '(("C-c w" . my-ctrl-c-w-map))))

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

;; Appearances and themes are loaded from their own file.


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

;; I use display-line-numbers, but only for programming modes.


(add-hook 'prog-mode-hook 'display-line-numbers-mode)



;; I also highlight the current line, only for programming modes.


(add-hook 'prog-mode-hook 'hl-line-mode)

;; Mode Line

;; Show in which function or method the point is.

;; Disabled for now until this problem with swift-mode is fixed:
;; https://github.com/swift-emacs/swift-mode/issues/157


(which-function-mode 0)



;; Show column numbers in the mode line.


(column-number-mode 1)



;; Show the current time.


(use-package time
  :ensure t
  :config
  (progn
    (setf display-time-default-load-average nil
          display-time-use-mail-icon t
          display-time-24hr-format nil)
    (display-time-mode t)))

;; Navigation Tree

;; Treemacs is a tree layout file explorer for Emacs:


(use-package treemacs
  :ensure t
  :commands treemacs
  :config
  (progn
    (setq treemacs-collapse-dirs              (if (executable-find "python") 3 0)
          treemacs-file-event-delay           5000
          treemacs-follow-after-init          t
          treemacs-follow-recenter-distance   0.1
          treemacs-goto-tag-strategy          'refetch-index
          treemacs-indentation                2
          treemacs-indentation-string         " "
          treemacs-is-never-other-window      nil
          treemacs-no-png-images              nil
          treemacs-project-follow-cleanup     nil
          treemacs-recenter-after-file-follow nil
          treemacs-recenter-after-tag-follow  nil
          treemacs-show-hidden-files          t
          treemacs-silent-filewatch           nil
          treemacs-silent-refresh             nil
          treemacs-sorting                    'alphabetic-desc
          treemacs-tag-follow-cleanup         t
          treemacs-tag-follow-delay           1.5
          treemacs-width                      35)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (executable-find "python3"))))
      (`(t . t)
       (treemacs-git-mode 'extended))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))



;; Integrate Treemacs with Projectile:


(use-package treemacs-projectile
  :ensure t
  :after treemacs projectile)

;; Pairs

;; To ease working with pairs, I use the smartparens package.


(use-package smartparens-config
  :commands smartparens-mode)



;; Highlight parentheses pairs.


(show-paren-mode 1)



;; Close pairs automatically.


(electric-pair-mode 1)



;; Rainbow-delimiters is a package which highlights delimiters such as
;; parentheses, brackets or braces according to their depth


(use-package rainbow-delimiters
  :ensure t
  :hook ((emacs-lisp-mode . rainbow-delimiters-mode)
         (ielm-mode . rainbow-delimiters-mode))
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

;; Cross References

;; Use ivy-xref to select cross references:


(use-package ivy-xref
  :ensure t
  :after ivy
  :init (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

;; Programming Language Customizations

;; These are my customizations for the programming languages I use most.

;; I generally dislike tabs in my programs.


(setq-default indent-tabs-mode nil)

;; C/C++/Objective-C/Objective-C++

;; For C languages, I use K&R style, with an indentation of 2 spaces.


(use-package cc-mode
  :config
  (add-hook 'c-mode-common-hook (lambda ()
                           (c-set-style "k&r")
                           (setq c-basic-offset 2)))
  ;; Enable LSP support.
  :hook ((c-mode c++-mode objc-mode) .
         (lambda () (require 'ccls) (lsp)))
  ;; Format with clang-format.
  :bind (:map c-mode-base-map
              ("C-c u" . clang-format)))



;; As there's not a specific Emacs mode for this programming language,
;; for Objective-C++ files, use Objective-C mode.


(add-to-list 'auto-mode-alist '("\\.mm$" . objc-mode))



;; Use LSP with company, and ccls as C++ client.


(use-package lsp-mode
  :load-path "~/Projects/lsp-mode"
  :bind (:map lsp-mode-map
              ("C-c C-d" . lsp-describe-thing-at-point))
  :commands lsp
  :config
  (setq lsp-prefer-flymake nil)
  (setq xref-prompt-for-identifier '(not xref-find-definitions
                                         xref-find-definitions-other-window
                                         xref-find-definitions-other-frame
                                         xref-find-references)))

(use-package lsp-sourcekit
  :after lsp-mode
  :load-path "~/Projects/lsp-sourcekit"
  :config
  (setenv "SOURCEKIT_TOOLCHAIN_PATH" "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain")
  (setq lsp-sourcekit-executable (expand-file-name "~/Projects/sourcekit-lsp/.build/release/sourcekit-lsp")))



;; LSP UI contains higher level UI modules for lsp-mode, like flycheck
;; support or code lenses.


(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-enable nil))

(use-package company-lsp
  :ensure t
  :commands company-lsp)

(use-package ccls
  :ensure t
  :diminish ccls-code-lens-mode
  :after lsp-mode
  :config
  (add-hook 'lsp-after-open-hook #'ccls-code-lens-mode)
  (setq ccls-executable (expand-file-name "~/Projects/ccls/Release/ccls")))

;; Clojure

;; Cider is the "de facto" package for working on Clojure projects.


(use-package cider
  :ensure t
  :defer t)

;; Djinni

;; Djinni is a IDL by Dropbox that helps generating interface code in C++/Objective-C++/Java.


(use-package djinni-mode
  :load-path "~/Projects/djinni-mode")

;; Elixir

;; Simple mode for working with Elixir files.


(use-package elixir-mode
  :ensure t
  :defer t)

;; Emacs Lisp

;; Suggest.el is a nice package that helps you discover Elisp functions
;; that do what you want.


(use-package suggest
  :ensure t
  :defer t)



;; Debug macros is easier with macrostep:


(use-package macrostep
  :ensure t
  :commands macrostep-mode)

;; Haskell

;; For Haskell I use haskell-mode.


(use-package haskell-mode
  :ensure t
  :defer t)

;; JavaScript

;; For JavaScript and other related web technologies, use web-mode:


(use-package web-mode
  :ensure t
  :mode
  (("\\.js\\'" . web-mode)
   ("\\.html?\\'" . web-mode)
   ("\\.phtml?\\'" . web-mode)
   ("\\.tpl\\.php\\'" . web-mode)
   ("\\.[agj]sp\\'" . web-mode)
   ("\\.as[cp]x\\'" . web-mode)
   ("\\.erb\\'" . web-mode)
   ("\\.mustache\\'" . web-mode)
   ("\\.djhtml\\'" . web-mode)
   ("\\.jsx$" . web-mode))
  :commands web-mode
  ;; Enable LSP support.
  :hook ((web-mode) . (lambda ()
                        ;; Set a local path to the Flow LSP binary.
                        (require 'lsp-clients)
                        (setq lsp-clients-flow-server (concat (projectile-project-root) "node_modules/.bin/flow"))
                        (lsp)))
  ;; Format code with Prettier.
  :bind (:map web-mode-map
              ("C-c u" . prettier)))



;; Also a minor mode for Flow:


(use-package flow-minor-mode
  :ensure t
  :hook ('web-mode . flow-minor-enable-automatically))

;; Kotlin

;; Use kotlin-mode for Kotlin development.


(use-package kotlin-mode
  :ensure t
  :defer t)

;; LaTeX

;; Use Auctex with tex-site for an excellent LaTeX environment. Also,
;; enable RefTeX mode whenever a LaTeX document is open.


(use-package tex-site
  :ensure auctex
  :hook ('LaTeX-mode . turn-on-reftex))

;; Markdown

;; I use markdown-mode to work on Markdown (.md) documents.


(use-package markdown-mode
  :ensure t
  :mode (("\\`README\\.md\\'" . gfm-mode))
  :init (setq markdown-command "multimarkdown"))



;; I want to fontify code blocks in Markdown:


(setq markdown-fontify-code-blocks-natively t)

;; PHP

;; Emacs does not come with a mode for editing PHP mode. Just use
;; php-mode from the package repository.


(use-package php-mode
  :ensure t)

;; Python

;; There are several packages for writing Python code. I use python.


(use-package python
  :ensure t
  :interpreter ("python" . python-mode))



;; Format Python code according to PEP8:


(use-package py-autopep8
  :ensure t
  :after python
  :bind
  (:map python-mode-map
        ("C-c u" . py-autopep8-buffer))
  :config
  (setq py-autopep8-options '("--max-line-length=79")))

;; Rust

;; Use rust-mode for editing Rust code:


(use-package rust-mode
  :ensure t
  :defer t)



;; For code completion and navigation use Racer (TODO: Move to lsp-mode):


(use-package racer
  :ensure t
  :after rust-mode
  :hook ((rust-mode . racer-mode)
         (racer-mode . eldoc-mode)
         (racer-mode . company-mode))
  :config
  (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
  (setq company-tooltip-align-annotations t))

;; Swift

;; I use swift-mode for Swift code.


(use-package swift-mode
  :ensure t
  :hook (swift-mode . (lambda () (lsp))))

;; TableGen

;; TableGen is an abstract IDL used by LLVM and related projects to
;; generate code automatically.


(use-package tablegen-mode
  :load-path "~/Projects/llvm-project/llvm/utils/emacs"
  :defer t)

;; Autocompletion

;; Autocompletion is very important for programming languages and natural
;; languages. I use company for that.


(use-package company
  :ensure t
  :diminish
  :hook (after-init . global-company-mode)
  :config
  (setq company-backends (delete 'company-semantic company-backends)))

;; Bazel

;; Bazel is a build system created by Google:


(use-package bazel-mode
  :ensure t
  :defer t)

;; CMake

;; CMake is a meta-build system that is commonly used in C++ projects.


(use-package cmake-mode
  :ensure t
  :mode ("CMakeLists.txt" "\\.cmake\\'"))



;; Enable type-aware highlighting support for CMake files:


(use-package cmake-font-lock
  :ensure t
  :hook (cmake-mode . cmake-font-lock-activate))

;; Code Formatting

;; Code formatting tools make smarter decisions than typical Emacs
;; indenters, specially for complex languages like C++. As yet, I use
;; clang-format for C++ and related languages.


(use-package reformatter
  :ensure t
  :after projectile
  :config
  ;; Clang-format (C/C++/Objective-C)
  (defconst clang-format-command "clang-format")
  (reformatter-define clang-format
    :program clang-format-command
    :lighter "Clang-format")

  ;; Prettier (JavaScript)
  (reformatter-define prettier
    :program (concat (projectile-project-root) "node_modules/.bin/prettier")
    :args (list "--stdin" "--stdin-filepath" buffer-file-name)
    :lighter "Prettier"))

;; Code Navigation

;; Sourcetrail is a great indexer to make sense of a big C/C++/Java
;; project.


(use-package sourcetrail
  :ensure t
  :bind ("C-c s" . sourcetrail-send-location))



;; For quick navigation inside a source file, I use ace-jump-mode.


(use-package ace-jump-mode
  :ensure t
  :bind ("C-c SPC" . ace-jump-mode))



;; Typically, I want to navigate quickly over the instances of a
;; particular symbol in a source file.


(use-package highlight-symbol
  :ensure t
  :bind (:map prog-mode-map
              ("M-n" . highlight-symbol-next)
              ("M-p" . highlight-symbol-prev)))

;; Code Selection

;; Use expand-region to increase the selected region by semantic units.


(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

;; Compiler Explorer

;; Rmsbolt is an offline alternative for Compiler Explorer:


(use-package rmsbolt
  :load-path "~/Projects/rmsbolt")

;; Copy as Format

;; I use a package to copy text from buffers in various formats:


(use-package copy-as-format
  :ensure t
  :bind (("C-c w m" . copy-as-format-markdown)
         ("C-c w g" . copy-as-format-slack)
         ("C-c w o" . copy-as-format-org-mode)
         ("C-c w r" . copy-as-format-rst)
         ("C-c w s" . copy-as-format-github)
         ("C-c w w" . copy-as-format))
  :init
  (setq copy-as-format-default "github"))

;; Cucumber

;; Enable syntax highlighting and indentation for Cucumber test files:


(use-package feature-mode
  :ensure t
  :mode (".feature$" . feature-mode))

;; Debugging

;; Debugging is very important when working on a program. I use RealGud,
;; which is a nice abstraction over several debuggers for programming
;; languages.


(use-package realgud
  :ensure t
  :disabled t)



;; I'm also exploring DAP (Debug Adapter Protocol). A protocol created by
;; Microsoft, similar to LSP, to interact with debuggers:


(use-package dap-mode
  :after lsp-mode
  :load-path "~/Projects/dap-mode"
  :config
  (dap-mode 1)
  (dap-ui-mode 1)
  (require 'dap-lldb))

;; Diminish

;; Use diminish.el to reduce the number of lighters to show in the mode line:


(use-package diminish
  :ensure t
  :demand t)

;; Directory Diffing

;; Use ztree for diffing two directories:


(use-package ztree
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

;; Edit Indirect

;; The edit-indirect package lets me edit source code in a separate buffer.


(use-package edit-indirect
  :ensure t
  :defer t)

;; Magit

;; Magit is the best Git porcelain I've ever used.


(use-package magit
  :ensure t
  :bind
  ("C-x g" . magit-status)
  :config
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules-overview
                          'magit-insert-unpulled-from-upstream)
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))



;; * Git Gutter


(use-package git-gutter
  :ensure t
  :custom
  (git-gutter:modified-sign "~")		; 
  (git-gutter:added-sign    "+")		; 
  (git-gutter:deleted-sign  "-")		; 
  :custom-face
  (git-gutter:modified ((t (:foreground "#f1fa8c" :background "#f1fa8c"))))
  (git-gutter:added    ((t (:foreground "#50fa7b" :background "#50fa7b"))))
  (git-gutter:deleted  ((t (:foreground "#ff79c6" :background "#ff79c6"))))
  :config
  (global-git-gutter-mode +1))

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

;; Forge

;; Forge is a package similar to Magithub:


(use-package forge
  :ensure t
  :after magit)

;; Google Test

;; For running Google Tests from a given buffer, I have created a simple
;; minor mode (must be enabled manually):


(require 'gtest-mode)

;; Helpful

;; Better help system.


(use-package helpful
  :ensure t
  :bind
  (
   ("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)
   ("C-c C-d" . helpful-at-point)
   ("C-h C" . helpful-command)))

;; Htmlize

;; Htmlize converts buffer text and decorations to HTML:


(use-package htmlize
  :ensure t
  :commands htmlize-buffer)

;; Image Editing

;; Blimp is a great wrapper for ImageMagick:


(use-package blimp
  :ensure t
  :hook (image-minor-mode . blimp-mode))

;; Ivy

;; Ivy is a lightweight completion framework.

;; Install counsel first:


(use-package counsel
  :ensure t
  :defer t)

(use-package counsel-projectile
  :ensure t
  :after counsel
  :init
  (counsel-projectile-mode)
  :config
  (setq counsel-find-file-ignore-regexp
      (concat
       ;; File names beginning with # or .
       "\\(?:\\`[#.]\\)"
       ;; File names ending with # or ~
       "\\|\\(?:\\`.+?[#~]\\'\\)")))



;; Smex is an enhancement for M-x.


(use-package smex
   :ensure t
   :after counsel)

(use-package ivy
  :ensure t
  :diminish
  :config
  (ivy-mode 1)

  ;; When switching buffers, offer recently accessed files that we don't
  ;; currently have open.
  (setq ivy-use-virtual-buffers t)

  (setq ivy-count-format "(%d/%d) ")

  ;; Don't require order, so 'func descr' matches 'describe-function'
  (setq ivy-re-builders-alist
        '((t . ivy--regex-ignore-order)))

  ;; Don't show ./ and ../ when finding files with ivy.
  ;; To go up a directory, use backspace.
  (setq ivy-extra-directories nil)

  ;; Highlight the current selection with an arrow too.
  (setq ivy-format-function 'ivy-format-function-arrow)

  ;; Don't start the search term with ^ by default. I often have a
  ;; substring in mind.
  (setq ivy-initial-inputs-alist nil)

  ;; Allow using the input as entered. This is useful when you want to
  ;; input a value that doesn't yet exist, such as creating a new file
  ;; with C-x C-f.
  (setq ivy-use-selectable-prompt t)
  :bind
  (
   ("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("<f1> f" . counsel-describe-function)
   ("<f1> v" . counsel-describe-variable)
   ("C-s" . swiper)
   ("<f7>" . counsel-imenu)
   ("M-y" . counsel-yank-pop)
   ("C-x b"   . ivy-switch-buffer)
   :map ivy-minibuffer-map
   ("M-y" . ivy-next-line)))

;; Use ido for projectile features, primarily C-x C-g (finding
;; files) and C-c p p (switching projects).
(require 'projectile)
(setq projectile-completion-system 'ivy)



;; Extend ivy with ivy-rich:


(use-package ivy-rich
  :ensure t
  :after ivy
  :config
  (ivy-rich-mode 1)
  (setq ivy-virtual-abbreviate 'full
        ivy-rich-switch-buffer-align-virtual-buffer t
        ivy-rich-path-style 'abbrev))

;; iOS Simulators

;; For accessing iOS simulator folders, I've created a simple minor mode:


(require 'ios-simulator)

;; Natural Languages

;; For checking spelling and grammar, I use an external Java tool: Language-tool.


(use-package langtool
  :ensure t
  :commands langtool-check-buffer
  :config
  (setq langtool-language-tool-jar "/usr/local/Cellar/languagetool/4.3/libexec/languagetool-commandline.jar"))

;; Org-Mode

;; Org-Mode configuration is handled in a separate file.


(require 'org-mode-config)

;; PDF Tools

;; I want a nice way to work on PDF documents graphically.

;; Install with `brew install pdf-tools`.


(use-package pdf-tools
  :ensure t
  :defer t
  :config
  (custom-set-variables
   '(pdf-tools-handle-upgrades nil))
  (setq pdf-info-epdfinfo-program "/usr/local/bin/epdfinfo")
  (pdf-tools-install))



;; pdf-linter will "lint" a PDF document using PDFBox Preflight app.


(use-package pdf-linter
  :load-path "~/.emacs.d/user-lisp/pdf-linter"
  :defer t
  :config
  (setq pdf-linter-jar "$HOME/PDFBox/preflight-app-2.0.12.jar"))



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
  :bind-keymap ("C-c p" . projectile-command-map))

;; Pandoc

;; Pandoc is a tool to convert between almost every document format.


(use-package pandoc-mode
  :ensure t
  :defer t)

;; Pass

;; I use Pass as password manager. Integrate it with Ivy:


(use-package ivy-pass
  :ensure t
  :commands ivy-pass)

;; REST

;; For making REST calls from Emacs, I use the convenient restclient package.


(use-package restclient
  :ensure t
  :defer t)

;; Search

;; For searching things, I use deadgrep, a nice interface over
;; ripgrep. Very fast.


(use-package deadgrep
  :ensure t
  :bind ("<f5>" . deadgrep))

;; Shell

;; Easy management of shell buffers.


(use-package shell-toggle
  :ensure t)

;; Snippets and Abbreviations

;; I use yasnippet for managing text snippets.


(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :init (yas-global-mode 1))

;; Syntax checking

;; I use flycheck for "on the fly" syntax checking.


(use-package flycheck
  :ensure t
  :defer t)



;; For linting packages intended to be published on MELPA, use flycheck-package:


(use-package flycheck-package
  :ensure t
  :after flycheck)

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

;; X.509

;; I've created a simple major mode that toggles between showing raw and
;; detailed information about a X.509 certificate.


(use-package x509-certificate-mode
  :defer t)

;; Xcode Projects

;; I've created a package for working on Xcode projects.


(use-package pbxproj-mode
  :load-path "~/.emacs.d/user-lisp/pbxproj-mode"
  :defer t)



;; I've also added on-the-fly syntax checking capabilities.


(use-package flycheck-pbxproj
  :load-path "~/.emacs.d/user-lisp/flycheck-pbxproj"
  :defer t)
