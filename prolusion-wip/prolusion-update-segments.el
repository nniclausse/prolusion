;; Version: $Id$
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'package)
(require 'prolusion-packages)

(defvar prolusion-upgrade-count nil)

(defun prolusion-upgrade-packages ()
  ""
  (interactive)
  (message "Upgrading prolusion packages")
  (package-refresh-contents)
  (save-window-excursion
    (package-list-packages t)
    (package-menu-mark-upgrades)
    (package-menu-execute t)))

(defun prolusion-packages/list-packages ()
  ""
  (interactive)
  (unless prolusion-upgrade-count
    (save-window-excursion
      (package-list-packages)
      (package-menu-mode)
      (setq prolusion-upgrade-count (length (package-menu--find-upgrades)))
      (kill-buffer (get-buffer "*Packages*"))))
  (if (> prolusion-upgrade-count 0)
      (progn
        (setq prolusion-upgrade-count-faced (format " %s" prolusion-upgrade-count))
        (propertize prolusion-upgrade-count-faced
                    'mouse-face 'mode-line-highlight
                    'help-echo (concat prolusion-upgrade-count-faced "\nmouse-1: Display minor mode menu")
                    'local-map (let ((map (make-sparse-keymap)))
                                 (define-key map
                                   [mode-line down-mouse-1]
                                   'prolusion-upgrade-packages)))
        prolusion-upgrade-count-faced)))

(message "%s" (prolusion-packages/list-packages))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; prolusion-updates-segment.el ends here
