; Emacs configuration file

;;;;;;;;;;;;;
;; GENERAL ;;
;;;;;;;;;;;;;

;; Add Emacs directory to load path
(add-to-list 'load-path user-emacs-directory)

;; Set some custom folders where binaries are located
(setq exec-path (append '("/usr/local/bin") exec-path))
(setenv "PATH" (concat "/usr/local/bin" ":" (getenv "PATH")))

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

;; Show line numbers on the left
(global-linum-mode t)

;; Format linum-mode output by appending a space at the end
(setq linum-format "%d ")

;; ido-mode
(ido-mode t)

;; recent mode
(recentf-mode 1)

;; Function definitions
(require 'defuns-config)

;;;;;;;
;; C ;;
;;;;;;;

;; Set k&r style
(setq c-default-style '((java-mode . "java")
			(awk-mode . "awk")
			(other . "k&r")))

;; Indent with 4 spaces
(setq c-basic-offset 4)
;; GMP info manual
(eval-after-load "info-look"
       '(let ((mode-value (assoc 'c-mode (assoc 'symbol info-lookup-alist))))
          (setcar (nthcdr 3 mode-value)
                  (cons '("(gmp)Function Index" nil "^ -.* " "\\>")
                        (nth 3 mode-value)))))


;;;;;;;;;;;;
;; PYTHON ;;
;;;;;;;;;;;;

(require 'python)
(define-key python-mode-map (kbd "\C-c\C-a") 'python-nav-beginning-of-block)
(define-key python-mode-map (kbd "\C-c\C-e") 'python-nav-end-of-block)

(setq python-check-command "pyflakes")

;;;;;;;;;;;;;;;;;
;; KEYBINDINGS ;;
;;;;;;;;;;;;;;;;;

(require 'keybindings)

;;;;;;;;;;;;;;
;; ORG-MODE ;;
;;;;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

(global-font-lock-mode 1)		     ; for all buffers
(add-hook 'org-mode-hook 'turn-on-font-lock)  ; org-mode buffers only
