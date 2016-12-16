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

(defvar prolusion-upgrade-count nil)

(defun prolusion-packages/list-packages ()
  ""
  (interactive)
  (message "so far so good 0")
  (unless prolusion-upgrade-count
    (save-window-excursion
      (package-list-packages)
      (package-menu-mode)
      (setq prolusion-upgrade-count (length (package-menu--find-upgrades)))
      (kill-buffer (get-buffer "*Packages*"))))
  (message "so far so good")
  (if (> prolusion-upgrade-count 0)
      (format " %s"
              (propertize prolusion-upgrade-count
                          'mouse-face 'mode-line-highlight
                          'help-echo (concat (prolusion-upgrade-count)
                                             "\nmouse-1: Display minor mode menu")
                          'local-map (let ((map (make-sparse-keymap)))
                                       (define-key map
                                         [mode-line down-mouse-1]
                                         (prolusion-upgrade-packages)))))))

(message "%s" (prolusion-packages/list-packages))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; prolusion-updates-segment.el ends here
