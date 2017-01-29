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

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tagging configuration
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq helm-gtags-ignore-case t)
(setq helm-gtags-auto-update t)
(setq helm-gtags-use-input-at-cursor t)
(setq helm-gtags-pulse-at-cursor t)
(setq helm-gtags-suggested-key-mapping t)
(setq helm-gtags-prefix-key "C-c t")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tagging requirements
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(prolusion/require-package 'helm-gtags)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tagging hooks
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook          'c-mode-hook 'helm-gtags-mode)
(add-hook        'c++-mode-hook 'helm-gtags-mode)
(add-hook 'emacs-lisp-mode-hook 'helm-gtags-mode)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tagging keybindings
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-eval-after-load 'helm-gtags
  (define-key helm-gtags-mode-map (kbd "h") 'helm-gtags-display-browser)
  (define-key helm-gtags-mode-map (kbd "P") 'helm-gtags-find-files)
  (define-key helm-gtags-mode-map (kbd "f") 'helm-gtags-parse-file)
  (define-key helm-gtags-mode-map (kbd "g") 'helm-gtags-find-pattern)
  (define-key helm-gtags-mode-map (kbd "s") 'helm-gtags-find-symbol)
  (define-key helm-gtags-mode-map (kbd "r") 'helm-gtags-find-rtag)
  (define-key helm-gtags-mode-map (kbd "t") 'helm-gtags-find-tag)
  (define-key helm-gtags-mode-map (kbd "d") 'helm-gtags-find-tag))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tagging modeline
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(diminish 'helm-gtags-mode)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'prolusion-tagging)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; prolusion-tagging.el ends here
