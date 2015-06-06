; Emacs configuration file

;;;;;;;;;;;;;
;; GENERAL ;;
;;;;;;;;;;;;;

;; Add Emacs subdirectories to load path
(setq main-src-path (concat user-emacs-directory "src/"))
(add-to-list 'load-path main-src-path)

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

; Highlight parentheses pairs
(show-paren-mode 1)
(set-face-background 'show-paren-match-face "#aaaaaa")
(set-face-attribute 'show-paren-match-face nil 
        :weight 'bold :underline nil :overline nil :slant 'normal)

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

;; Function definitions
(require 'defuns-config)

;;;;;;;
;; C ;;
;;;;;;;

;; Set k&r style
(setq c-default-style '((java-mode . "java")
			(awk-mode . "awk")
			(other . "k&r")))
;; Indent with 4 spaces, no tabs
(setq c-basic-offset 4)
(setq-default indent-tabs-mode nil)

;; Make RET indent automatically
(defun my-make-CR-do-indent ()
  (define-key c-mode-base-map "\C-m" 'c-context-line-break))
(add-hook 'c-initialization-hook 'my-make-CR-do-indent)

;; GMP info manual
(eval-after-load "info-look"
       '(let ((mode-value (assoc 'c-mode (assoc 'symbol info-lookup-alist))))
          (setcar (nthcdr 3 mode-value)
                  (cons '("(gmp)Function Index" nil "^ -.* " "\\>")
                        (nth 3 mode-value)))))


;;;;;;;;;;;;
;; PYTHON ;;
;;;;;;;;;;;;

(setq python-mode-path (concat user-emacs-directory "python-mode/"))
(add-to-list 'load-path python-mode-path)

(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

;;;;;;;;;;;;;;
;; MARKDOWN ;;
;;;;;;;;;;;;;;

(setq markdown-mode-path (concat user-emacs-directory "markdown-mode/"))
(add-to-list 'load-path markdown-mode-path)

(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;;;;;;;;;;;;;;
;;   SWIFT   ;;
;;;;;;;;;;;;;;;

(setq swift-mode-path (concat user-emacs-directory "swift-mode/"))
(add-to-list 'load-path swift-mode-path)

(require 'swift-mode)

;;;;;;;;;;;;;;;
;;   LATEX   ;;
;;;;;;;;;;;;;;;

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

;;;;;;;;;;;;
;; OCTAVE ;;
;;;;;;;;;;;;

(setq auto-mode-alist
      (cons
       '("\\.m$" . octave-mode)
       auto-mode-alist))

;;;;;;;;;;;;;;
;; ORG-MODE ;;
;;;;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

(global-font-lock-mode 1)		     ; for all buffers
(add-hook 'org-mode-hook 'turn-on-font-lock)  ; org-mode buffers only

;;;;;;;;;;;;;;;
;; YASNIPPET ;;
;;;;;;;;;;;;;;;

(setq yasnippet-path (concat user-emacs-directory "plugins/yasnippet/"))
(add-to-list 'load-path yasnippet-path)

(require 'yasnippet)
(yas-global-mode 1)

;;;;;;;;;;;;;;;
;;    HELM   ;;
;;;;;;;;;;;;;;;

(setq async-path (concat user-emacs-directory "plugins/async/"))
(setq helm-path (concat user-emacs-directory "plugins/helm/"))
(add-to-list 'load-path async-path)
(add-to-list 'load-path helm-path)

(require 'helm-config)
(helm-mode 1)

;;;;;;;;;;;;;;;
;;   ELDoc   ;;
;;;;;;;;;;;;;;;

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;;;;;;;;;;;;;;;
;;   ASPELL  ;;
;;;;;;;;;;;;;;;

(setq-default ispell-program-name "aspell")

;;;;;;;;;;;;;;;;;;
;;   COCOAPODS  ;;
;;;;;;;;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("\\.podspec$" . ruby-mode))

;;;;;;;;;;;;;;;;;
;; JAVASCRIPT  ;;
;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;;;;;;;;;;;;;;;;;
;; KEYBINDINGS ;;
;;;;;;;;;;;;;;;;;

(require 'keybindings)
