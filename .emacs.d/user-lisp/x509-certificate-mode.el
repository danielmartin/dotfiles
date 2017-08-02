;;; x509-certificate-mode.el --- Major mode for working with X.509 certificates.

;;; Commentary:

;;; Code:

(defvar x509-certificate-mode-hook nil)

(defun x509-certificate-parse-command (encoding)
  "Parse the buffer using OpenSSL's x509 command."
  (let ((errbuf (generate-new-buffer-name "*x509-parse-error*")))
    ;; Try to parse the buffer using the specified encoding.
    (shell-command-on-region
     (point-min)
     (point-max)
     (format "openssl x509 -text -noout -inform %s" encoding)
     (buffer-name)
     t
     errbuf)
    ;; Check for failure, represented by the existence of errbuf.
    (if (get-buffer errbuf)
        ;; Restore the original state of the buffer.
        (progn
          (kill-buffer errbuf)
          (insert x509-certificate-mode-text)
          nil)
      t)))

(defun x509-certificate-parse ()
  "Parse the buffer as a certificate, trying multiple encodings."
  (interactive)
  (if (not (eq x509-certificate-mode-display :raw))
      (error "The buffer is not in :raw mode, it's in %s mode."
             x509-certificate-mode-display)
    (let ((modified (buffer-modified-p)))
      ;; Save the contents of the buffer.
      (setq x509-certificate-mode-text (buffer-string))
      (read-only-mode -1)
      ;; Try to convert the buffer through different formats.
      (if (not (x509-certificate-parse-command "pem"))
          (if (not (x509-certificate-parse-command "der"))
              (error "Failed to parse buffer as X.509 certificate.")))
      (read-only-mode 1)
      ;; Restore previous modification state.
      (set-buffer-modified-p modified)
      (setq x509-certificate-mode-display :parsed))))

(defun x509-certificate-raw ()
  "Revert buffer to unparsed contents."
  (interactive)
  (if (not (eq x509-certificate-mode-display :parsed))
      (error "The buffer is not in :parsed mode, it's in %s mode."
             x509-certificate-mode-display)
    (let ((modified (buffer-modified-p)))
      ;; Delete the buffer, which currently contains the parsed format.
      (read-only-mode -1)
      (erase-buffer)
      ;; Convert the buffer into its raw format.
      (insert x509-certificate-mode-text)
      (read-only-mode 1)
      ;; Restore previous modification state.
      (set-buffer-modified-p modified)
      (setq x509-certificate-mode-display :raw))))

(defun x509-certificate-toggle-display ()
  "Toggle between raw and parsed displays of the buffer."
  (interactive)
  (cond ((eq x509-certificate-mode-display :parsed)
         (x509-certificate-raw))
        ((eq x509-certificate-mode-display :raw)
         (x509-certificate-parse))
        (t
         (error "Variable x509-certificate-display is in an unknown state: %s"
                x509-certificate-mode-display))))

(defvar x509-certificate-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-c C-c") 'x509-certificate-toggle-display)
    map)
  "Keymap for X.509 certificate major mode")

(add-to-list 'auto-mode-alist '("\\.\\(der\\|crt\\|pem\\)$" . x509-certificate-mode))
(add-to-list 'magic-mode-alist '("-----BEGIN CERTIFICATE-----" . x509-certificate-mode))

(defun x509-certificate-mode ()
  "Major mode for viewing X.509 certificates"
  (interactive)
  ;; Clear the slate.
  (kill-all-local-variables)
  ;; Use our keymap just for this buffer.
  (use-local-map x509-certificate-mode-map)
  (setq major-mode 'x509-certificate-mode
        mode-name "X.509")
  (defvar-local x509-certificate-mode-display :raw
    "Current display mode of the data.")
  (defvar-local x509-certificate-mode-text nil
    "Original text of the buffer.")
  ;; Run customization hooks.
  (run-hooks 'x509-certificate-mode-hook)
  ;; Parse the buffer.
  (x509-certificate-parse))

(provide 'x509-certificate-mode)
