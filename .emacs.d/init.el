;; Default Shell

;; macOS Catalina has changed the default SHELL environment variable to
;; zsh, but let's still use Bash as the default shell in Emacs:


(setq shell-file-name "/bin/bash")

;; Customization File

;; Save customizations in a separate file:


(setq custom-file "~/.emacs.d/settings.el")
(load custom-file)

;; Measuring Startup Time

;; Declare a constant so that we can measure how long it takes to load the
;; initialization files:


(defconst emacs-start-time (current-time))

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
        ("melpa" . "https://melpa.org/packages/")))
(package-initialize)



;; Bootstrap use-package:


(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))



;; Add an environment variable required by the PDF Tools package:


(setenv "PKG_CONFIG_PATH" "/usr/local/lib/pkgconfig:/usr/local/Cellar/libffi/3.2.1/lib/pkgconfig")

;; General Customizations

;; Increase a bit garbage collection threshold:


(setq gc-cons-threshold 1600000)



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

;; Appearance and Themes

;; Appearances and themes are loaded from their own file.


(require 'appearance)

;; Compilation Mode

;; Some customizations to Emacs compilation mode:


(use-package compile
  :defer t
  :config
  (setq compilation-scroll-output t))

;; Cross References

;; Use ivy-xref to select cross references:


(use-package ivy-xref
  :ensure t
  :after ivy
  :init (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

;; Custom Keymaps

;; Define my custom prefix keys:


(eval-and-compile
  (mapc #'(lambda (entry)
            (define-prefix-command (cdr entry))
            (bind-key (car entry) (cdr entry)))
        '(("C-c w" . my-ctrl-c-w-map))))

;; Ediff

;; I like Ediff's control panel to show in the same frame, even on
;; graphical environments.


(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Fast Scroll

;; To ensure scrolling is fast in Emacs, I use a package that disables
;; non-essential things while the window is being scrolled:


(use-package fast-scroll
  :ensure t
  :demand t
  :config
  (add-hook 'fast-scroll-start-hook (lambda () (flycheck-mode -1)))
  (add-hook 'fast-scroll-end-hook (lambda () (flycheck-mode 1)))
  (fast-scroll-config)
  (fast-scroll-mode 1))

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

;; Mode Line

;; Show in which function or method the point is.


(which-function-mode 1)



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



;; Integrate Treemacs with LSP:


(use-package lsp-treemacs
  :load-path "~/Projects/lsp-treemacs"
  :after treemacs lsp-mode)

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


(add-hook 'before-save-hook
          (lambda ()
            ;; Don't delete trailing whitespace in PDFs to avoid
            ;; corrupting them.
            (let ((extension (file-name-extension buffer-file-name)))
              (unless (and extension (string= "pdf" (downcase extension)))
                (delete-trailing-whitespace)))))

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
  :hook ((c-mode-common . (lambda ()
                           (c-set-style "k&r")
                           (setq c-basic-offset 2)
                           (subword-mode 1)))
         (c++-mode . gtest-mode))
  ;; Format with clang-format.
  :bind (:map c-mode-base-map
              ("C-c u" . clang-format)))



;; As there's not a specific Emacs mode for this programming language,
;; for Objective-C++ files, use Objective-C mode.


(add-to-list 'auto-mode-alist '("\\.mm$" . objc-mode))



;; Use LSP with company, and clangd as C++ client.


(use-package lsp-mode
  :ensure t
  :bind (:map lsp-mode-map
              ("C-c C-d" . lsp-describe-thing-at-point))
  :commands lsp
  :hook ((c-mode-common . (lambda () (lsp-deferred)))
         (swift-mode . lsp-deferred)
         (js2-mode . (lambda ()
                       ;; Set a local path to the Flow LSP binary.
                       (require 'lsp-clients)
                       (setq lsp-clients-flow-server (concat (projectile-project-root) "node_modules/.bin/flow"))
                       (lsp-deferred)))
         (sh-mode . lsp-deferred))
  :config
  ;; Prefer Flycheck to Flymake.
  (setq lsp-prefer-flymake nil)
  ;; Configure clangd custom path.
  (setq lsp-clients-clangd-executable "~/Projects/llvm-project/build-Release/bin/clangd")
  ;; Do not prompt for identifier when querying definitions/references.
  (setq xref-prompt-for-identifier '(not xref-find-definitions
                                         xref-find-definitions-other-window
                                         xref-find-definitions-other-frame
                                         xref-find-references))
  (setq lsp-enable-links nil))



;; Some additional lsp-clients configuration


(use-package lsp-clients
  :config
  ;; Custom signature extraction logic to use with Clangd.
  (cl-defmethod lsp-clients-extract-signature-on-hover (contents (_server-id (eql clangd)))
    "Extract a representative line from clangd's CONTENTS, to show in the echo area.
This function tries to extract the type signature from CONTENTS,
or the first line if it cannot do so. A single line is always
returned to avoid that the echo area grows uncomfortably."
    (with-temp-buffer
      (-let [value (lsp:markup-content-value contents)]
        (insert value)
        (goto-char (point-min))
        (if (re-search-forward (rx (seq "```cpp\n"
                                        (opt (group "//"
                                                    (zero-or-more nonl)
                                                    "\n"))
                                        (group
                                         (one-or-more
                                          (not (any "`")))
                                         "\n")
                                        "```")) nil t nil)
            (progn (narrow-to-region (match-beginning 2) (match-end 2))
                   (let ((foo (lsp-join-region (point-min) (point-max))))
                     (widen)
                     (goto-char (point-min))
                     (if (re-search-forward (rx (seq "Passed"
                                                     (group (zero-or-more nonl))
                                                     " as "
                                                     (group (zero-or-more nonl)))) nil t nil)
                         (progn (let ((param-pass
                                       (pcase (buffer-substring (match-beginning 1) (match-end 1))
                                         (" by const reference" (match-string 0))
                                         (" by reference" (propertize
                                                           (match-string 0)
                                                           'face '(:foreground "red")))
                                         (_ (propertize
                                             (match-string 0)
                                             'face '(:foreground "blue"))))))
                                  (concat (lsp--render-element foo) " (" (string-trim param-pass) ")")))
                       (lsp--render-element foo))))
          (car (s-lines (lsp--render-element contents))))))))

(use-package lsp-sourcekit
  :ensure t
  :after lsp-mode
  :config
  (setq lsp-sourcekit-executable (expand-file-name "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/sourcekit-lsp")))



;; LSP UI contains higher level UI modules for lsp-mode, like flycheck
;; support or code lenses.


(use-package lsp-ui
  :load-path "~/Projects/lsp-ui"
  :after lsp-mode
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-enable nil)
  ;; I'll work on a better way to show documentation, disable
  ;; `lsp-ui-doc-mode' for now.
  (setq lsp-ui-doc-enable nil)
  :bind (:map lsp-mode-map
              ("C-c C-b" . lsp-ui-doc-glance)))

(use-package company-lsp
  :ensure t
  :after lsp-mode
  :commands company-lsp)



;; Disable ccls for now as I'm evaluating clangd.


(use-package ccls
  :disabled t
  :load-path "~/Projects/emacs-ccls"
  :diminish ccls-code-lens-mode
  :after lsp-mode
  :config
  (setq ccls-executable (expand-file-name "~/Projects/ccls/Release/ccls")))



;; Clang-tidy integration is disabled for now, as clangd already includes it.


(use-package flycheck-clang-tidy
  :disabled t
  :after lsp-ui-flycheck
  :hook
  (flycheck-mode . flycheck-clang-tidy-setup)
  :config
  (setq flycheck-clang-tidy-executable "~/Projects/llvm-project/build-Release/bin/clang-tidy")
  ;; .clang-tidy and compile_commands.json should always be in the
  ;; same folder.
  (setq flycheck-clang-tidy-build-path ".")
  (flycheck-add-next-checker 'lsp-ui '(warning . c/c++-clang-tidy)))



;; Search for workspace symbols using ivy:


(use-package lsp-ivy
  :ensure t
  :after lsp-mode
  :bind (:map lsp-mode-map
              ("C-c w s" . lsp-ivy-workspace-symbol)))

;; Clojure

;; Cider is the "de facto" package for working on Clojure projects.


(use-package cider
  :ensure t
  :defer t)

;; Djinni

;; Djinni is a IDL by Dropbox that helps generating interface code in C++/Objective-C++/Java.


(use-package djinni-mode
  :ensure t
  :load-path "~/Projects/djinni-mode"
  :mode ("\\.djinni\\'" . djinni-mode))

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



;; Debugging macros is easier with macrostep:


(use-package macrostep
  :ensure t
  :commands macrostep-mode)

;; Haskell

;; For Haskell I use haskell-mode.


(use-package haskell-mode
  :ensure t
  :defer t)

;; JavaScript

;; For JavaScript, use js2-mode:


(use-package js2-mode
  :ensure t
  :mode
  (("\\.js\\'" . js2-mode))
  :init
  ;; Parsing problems and warnings are handled by the JS language
  ;; server.
  (setq js2-mode-show-parse-errors nil)
  (setq js2-mode-show-strict-warnings nil)
  ;; Format with Prettier.
  :bind (:map js2-mode-map
              ("C-c u" . prettier)))



;; Also a minor mode for Flow:


(use-package flow-minor-mode
  :ensure t
  :hook ('js2-mode . flow-minor-enable-automatically))

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
  :init (setq markdown-command "multimarkdown")
  :config (setq markdown-reference-location 'end))



;; I want to fontify code blocks in Markdown:


(setq markdown-fontify-code-blocks-natively t)

;; PHP

;; Emacs does not come with a mode for editing PHP mode. Just use
;; php-mode from the package repository.


(use-package php-mode
  :ensure t
  :defer t)

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
  :defer t
  :config
  ;; `magit-which-function' may display a method as
  ;; "Class.method". That won't work for
  ;; `magit-log-trace-definition'. Customize
  ;; `magit-log-trace-definition-function' to keep only the last part
  ;; after ".".
  (setq-local magit-log-trace-definition-function
              (lambda ()
                (cadr (split-string (magit-which-function) "\\.")))))



;; Swift-helpful is a mode that provides a self-documenting experience for writing Swift code:


(use-package swift-helpful
  :load-path "~/Projects/swift-helpful"
  :after swift-mode
  :config
  (setq swift-helpful-stdlib-path "~/Projects/swift-source/swift/stdlib/public/"))

;; TableGen

;; TableGen is an abstract IDL used by LLVM and related projects to
;; generate code automatically.


(use-package tablegen-mode
  :load-path "~/Projects/llvm-project/llvm/utils/emacs"
  :mode ("\\.td\\'"))

;; Tree-sitter

;; Tree-sitter is a generic parser of programming languages that can complement
;; Emacs's specific programming modes and Language Server Protocol.


(use-package tree-sitter
  :disabled t
  :load-path "~/Projects/emacs-tree-sitter"
  :config
  (add-to-list 'tree-sitter-major-mode-language-alist '(swift-mode . swift))
  (add-hook 'swift-mode-hook #'tree-sitter-mode))

;; TypeScript

;; Add support for editing TypeScript files with `typescript-mode`:


(use-package typescript-mode
  :ensure t
  :defer t)

;; YAML

;; Add support for editing YAML files with `yaml-mode`:


(use-package yaml-mode
  :ensure t
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

;; Certificate Handling

;; I use a major mode for viewing certificates, CRLs, keys, ASN.1, etc.


(use-package x509-mode
  :ensure t
  :defer
  :config
  (setq x509-openssl-cmd "/usr/local/opt/openssl/bin/openssl"))

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
  :ensure t
  :defer t
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
  :ensure t
  :commands dap-mode
  :hook (dap-stopped . (lambda (arg) (call-interactively #'dap-hydra)))
  :config
  (dap-mode 1)
  (require 'dap-ui)
  (dap-ui-mode 1)
  (require 'dap-lldb))

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

;; Feeds

;; For browsing feeds, I use Elfeed:


(use-package elfeed
  :ensure t
  :defer t
  :bind
  ("C-x w" . elfeed)
  :config
  (setq elfeed-feeds
        '(("http://nullprogram.com/feed/" programming)
          ("https://flak.tedunangst.com/rss" programming)
          ("http://planet.emacsen.org/atom.xml" emacs)
          ("https://nvd.nist.gov/feeds/xml/cve/misc/nvd-rss-analyzed.xml" security)
          ("https://steipete.wtf/feed.xml" ios)
          ("https://github.blog/feed/atom" git))))

;; Browse at Remote

;; This package browses target pages at GitHub/Bitbucket.


(use-package browse-at-remote
  :ensure t
  :bind
  ("C-c g g" . dm/browse-at-remote)
  :config
  (defun dm/browse-at-remote (arg)
    "Call `browse-at-remote' with `browse-at-remote-prefer-symbolic' set to nil.
With a prefix argument, call regular `browse-at-remote'. The
difference `browse-at-remote-prefer-symbolic' does is that, when
set to nil, the URL will reference a commit hash instead of a
particular branch, so it will be completely stable over time."
    (interactive "P")
    (if arg
        (browse-at-remote)
      (let ((browse-at-remote-prefer-symbolic nil))
        (browse-at-remote)))))

;; Diff-hl

;; Diff-hl highlight uncommitted changes.


(use-package diff-hl
  :ensure t
  :init
  (global-diff-hl-mode)
  :hook (magit-post-refresh . diff-hl-magit-post-refresh))

;; Forge

;; Forge is a package similar to Magithub:


(use-package forge
  :ensure t
  :after magit)

;; Git TimeMachine

;; git-timemachine is a package that intuitively shows previous versions
;; of a particular file from a Git repository.


(use-package git-timemachine
  :ensure t
  :defer t)

;; Git Undo

;; Git-undo lets you select a region and revert changes in that region to
;; the most recent Git historical version.


(use-package git-undo
  :load-path "~/.emacs.d/user-lisp/git-undo"
  :commands git-undo)

;; GitHub Review

;; Perform code reviews from the comfort of Emacs:


(use-package github-review
  :ensure t
  :after forge
  :config
  (transient-insert-suffix 'forge-dispatch "c p"
    '("c r" "github-review" github-review-forge-pr-at-point)))

;; Magit

;; Magit is the best Git porcelain I've ever used.


(use-package magit
  :ensure t
  :bind
  ("C-x g" . magit-status)
  :config
  ;; I want to highlight whitespace errors in both added and removed
  ;; lines.
  (setq magit-diff-paint-whitespace-lines 'both)
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules-overview
                          'magit-insert-unpulled-from-upstream)
  (magit-add-section-hook 'magit-status-sections-hook
                          #'forge-insert-assigned-pullreqs
                          nil t)
  ;; I sometimes work on a few branches simultaneously and switching
  ;; between them when they are shown in alphabetic order is not very
  ;; convenient, I may forget the exact name of those branches. Create
  ;; a custom Transient command to checkout a branch with the list
  ;; sorted by creation date.
  (defun dm/magit-checkout ()
    "Like `magit-checkout', but shows refs sorted by creation date."
    (interactive)
    (let ((magit-list-refs-sortby "-creatordate"))
      (call-interactively 'magit-checkout)))

  (transient-insert-suffix 'magit-branch "b"
    '("B" "branch (sorted by date)" dm/magit-checkout)))



;; Integrate Magit with git-imerge:


(use-package magit-imerge
  :ensure t
  :after magit)

;; Google Test

;; For running Google Tests from a given buffer, I have created a simple
;; minor mode:


(use-package gtest-mode
  :after cc-mode)

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
   ("C-s" . swiper-isearch)
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

;; PDF Debugger

;; I have created a specialized mode for debugging PDFs:


(use-package pdf-debugger
  :defer t
  :load-path "~/Projects/PSPDFKit/tools/pdf-debugger")

;; PSPDFKit Changelog

;; I have created a specialized mode for adding entries to the PSPDFKit changelog:


(use-package pspdf-changelog
  :after forge
  :load-path "~/Projects/PSPDFKit/tools")

;; Regular Expressions

;; Use the xr package to convert Elisp regexps to more readable rx forms:


(use-package xr
  :ensure t
  :defer t)



;; Use relint to detect regular expression errors in Emacs Lisp files:


(use-package relint
  :ensure t
  :defer t)

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
  :ensure t
  :defer t)

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

;; Wgrep

;; Wgrep (writable grep) is a package to make grep buffers editable. It's
;; also used by ivy-occur buffers.


(use-package wgrep
  :ensure t
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

;; Initialization Time

;; Inform in the echo area how long it took to load this file:


(let ((elapsed (float-time (time-subtract (current-time)
                                          emacs-start-time))))
  (message "Loading %s...done (%.3fs)" load-file-name elapsed))

(add-hook 'after-init-hook
          `(lambda ()
             (let ((elapsed
                    (float-time
                     (time-subtract (current-time) emacs-start-time))))
               (message "Loading %s...done (%.3fs) [after-init]"
                        ,load-file-name elapsed))) t)
