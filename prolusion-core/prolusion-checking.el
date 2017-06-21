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

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Checking requirements
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(prolusion/require-package 'flycheck)
(prolusion/require-package 'flycheck-irony)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Checking setup
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq flycheck-indication-mode 'left-fringe)

(add-to-list 'flycheck-emacs-lisp-load-path prolusion-dir)
(add-to-list 'flycheck-emacs-lisp-load-path prolusion-core-dir)

(when (fboundp 'define-fringe-bitmap)
  (define-fringe-bitmap 'prolusion-flycheck-fringe-indicator
    (vector #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00111000
            #b01111100
            #b01111100
            #b01111100
            #b00111000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000)))

(flycheck-define-error-level 'error
  :overlay-category 'flycheck-error-overlay
  :fringe-bitmap 'prolusion-flycheck-fringe-indicator
  :fringe-face 'flycheck-fringe-error)

(flycheck-define-error-level 'warning
  :overlay-category 'flycheck-warning-overlay
  :fringe-bitmap 'prolusion-flycheck-fringe-indicator
  :fringe-face 'flycheck-fringe-warning)

(flycheck-define-error-level 'info
  :overlay-category 'flycheck-info-overlay
  :fringe-bitmap 'prolusion-flycheck-fringe-indicator
  :fringe-face 'flycheck-fringe-info)

(setq flycheck-emacs-lisp-load-path 'inherit)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Checking hooks
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-after-load 'flycheck '(add-hook 'flycheck-mode-hook 'flycheck-irony-setup))

(add-hook          'c-mode-hook 'flycheck-mode)
(add-hook        'c++-mode-hook 'flycheck-mode)
(add-hook       'objc-mode-hook 'flycheck-mode)
(add-hook     'python-mode-hook 'flycheck-mode)
(add-hook 'emacs-lisp-mode-hook 'flycheck-mode)
(add-hook        'js2-mode-hook 'flycheck-mode)

(setq-default flycheck-emacs-lisp-load-path load-path)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Checking modeline
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-after-load 'flycheck '(diminish 'flycheck-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'prolusion-checking)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; prolusion-checking.el ends here
