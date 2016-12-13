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

(require 'async)
(require 'package)

;; (async-start
;;  (lambda ()
     (save-excursion
     (package-list-packages t)
     (setq prolusion--packages-upgrade-count (package-menu--find-upgrades))
     (kill-buffer "*Packages*")
 ;;     prolusion--packages-upgrade-count))

 ;; (lambda (prolusion--packages-upgrade-count)
(message (format "%s" (length prolusion--packages-upgrade-count))))
;; ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; prolusion-updates-segment.el ends here
