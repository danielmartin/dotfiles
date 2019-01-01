;;; ios-simulator.el --- iOS simulator support.

;;; Commentary:
;; iOS simulator helpers.


;;; Code:

(require 'f)
(require 'dash)
(require 'simple)

(defun dm/ios-sim-booted-id ()
  "Booted simulator ID."
  (nth 0 (split-string (shell-command-to-string "xcrun simctl list devices | grep Booted | sed -n 's/^.*\\([A-F0-9]\\{8\\}-\\([A-F0-9]\\{4\\}-\\)\\{3\\}[A-F0-9]\\{12\\}\\).*$/\\1/p'") "\n" t)))

(defun dm/ios-sim-package-id-in-dir (dir)
  "Get package ID from directory."
  (nth 0 (process-lines "/usr/libexec/PlistBuddy"
                        "-c" "Print :MCMMetadataIdentifier" (format "%s/.com.apple.mobile_container_manager.metadata.plist" dir))))

(defun dm/ios-sim-app-directories ()
  "Get all app directories for booted simulator."
  (let ((booted-sim-id (dm/ios-sim-booted-id)))
    (assert booted-sim-id nil "No booted simulator")
    (f-directories (expand-file-name (format "~/Library/Developer/CoreSimulator/Devices/%s/data/Containers/Data/Application" booted-sim-id)))))

(defun dm/ios-sim-app-bundle-ids ()
  "Get all app bundle IDs for booted simulator."
  (-map (lambda (dir)
          (dm/ios-sim-package-id-in-dir dir))
        (dm/ios-sim-app-directories)))

(defun dm/ios-sim-app-directory (bundle-id)
  "Get app directory from BUNDLE-ID."
  (-first (lambda (dir)
            (equal bundle-id
                   (dm/ios-sim-package-id-in-dir dir)))
          (dm/ios-sim-app-directories)))

(defvar dm/ios-sim--last-bundle-id nil)

(defun dm/ios-sim-documents-dir (arg)
  "Show contents of documents directory for app. With ARG, show completion."
  (interactive "P")
  (let* ((bundle-id (if (or arg (not dm/ios-sim--last-bundle-id))
                        (completing-read "Bundle ID: " (dm/ios-sim-app-bundle-ids))
                      (read-string
                       (if dm/ios-sim--last-bundle-id
                           (format "Bundle ID (%s): " dm/ios-sim--last-bundle-id)
                         "Bundle ID: "))))
         (path (dm/ios-sim-app-directory (if (> (length bundle-id) 0)
                                             bundle-id
                                           dm/ios-sim--last-bundle-id))))
    (assert path nil "Not found")
    (when (> (length bundle-id) 0)
      (setq dm/ios-sim--last-bundle-id bundle-id))
    (find-file path)))

(provide 'ios-simulator)

;;; ios-simulator.el ends here
