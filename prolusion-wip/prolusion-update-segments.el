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

(defun prolusion-packages/list-packages ()
  ""
  (interactive)
  (save-window-excursion
    (package-list-packages)
    (package-menu-mode)
    (let ((prolusion--packages-upgrade-count (length (package-menu--find-upgrades)))
      (kill-buffer (get-buffer "*Packages*")))
      prolusion--packages-upgrade-count)))

(message " %s" (prolusion-packages/list-packages))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; prolusion-updates-segment.el ends here
