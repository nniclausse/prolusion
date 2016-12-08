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
;; Eshell requirements
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(prolusion-require-package         'multi-eshell)
(prolusion-require-package 'exec-path-from-shell)
(prolusion-require-package 'eshell-prompt-extras)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Eshell setup
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq eshell-directory-name                                                       prolusion-save-dir)
(setq eshell-history-file-name       (expand-file-name "prolusion-eshell-history" prolusion-save-dir))
(setq eshell-last-dir-ring-file-name (expand-file-name "prolusion-eshell-lastdir" prolusion-save-dir))

(when (memq window-system '(mac ns))
  (setq exec-path-from-shell-arguments (quote ("-l")))
  (setq exec-path-from-shell-variables (quote ("PATH" "MANPATH" "CMAKE_PREFIX_PATH" "LC_ALL" "LANG" "LC_CTYPE")))
       (exec-path-from-shell-initialize))

(setq multi-eshell-name "*eshell*")
(setq multi-eshell-shell-function (quote (eshell)))

(with-eval-after-load "esh-opt"
  (autoload 'epe-theme-lambda "eshell-prompt-extras")
  (setq eshell-highlight-prompt nil
        eshell-prompt-function 'epe-theme-dakrone))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Eshell functions
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prolusion-eshell-clear-buffer ()
  "Clear eshell buffer"
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Eshell hooks
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'eshell-mode-hook 'prolusion-eshell-no-highlight)

(add-hook 'eshell-mode-hook (lambda () (setq-local global-hl-line-mode nil)))
(add-hook   'term-mode-hook (lambda () (setq-local global-hl-line-mode nil)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Eshell keybindings
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-c l l") 'multi-eshell)
(global-set-key (kbd "C-c l o") 'multi-eshell-switch)
(global-set-key (kbd "C-c l O") 'multi-eshell-go-back)
(global-set-key (kbd "C-c l c") 'prolusion-eshell-clear-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'prolusion-eshell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; prolusion-eshell.el ends here
