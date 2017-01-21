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
;; Editor requirements
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(prolusion/require-package 'rainbow-delimiters)
(prolusion/require-package 'smartparens)
(prolusion/require-package 'whitespace)
(prolusion/require-package 'undo-tree)
(prolusion/require-package 'header2)
(prolusion/require-package 'iedit)
(prolusion/require-package 'helm)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editor setup
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(prefer-coding-system 'utf-8)

(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)

(require 'smartparens-config)

(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)
(setq-default tab-width 4)

(setq whitespace-line-column 80)
(setq whitespace-style '(face tabs empty trailing lines-tail))

(global-undo-tree-mode)

     (smartparens-global-mode t)
(show-smartparens-global-mode t)

(setq header-file-name 'buffer-file-name)
(setq make-header-hook
  '(header-rcs-id
    header-blank
    header-end-line
    header-commentary
    header-blank
    header-end-line
    header-history
    header-blank
    header-end-line
    header-code
    header-eof))

(when (display-graphic-p)
  (global-hl-line-mode +1))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editor functions
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prolusion/duplicate-line ()
  ""
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editor hooks
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook   'find-file-hook (lambda () (whitespace-mode +1)))
(add-hook 'before-save-hook (lambda () (whitespace-cleanup)))

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editor modeline
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(diminish 'smartparens-mode)
(diminish  'whitespace-mode)
(diminish   'undo-tree-mode)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editor keybindings
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-c e s") 'ff-find-other-file)
(global-set-key (kbd "C-c e m") 'make-header)
(global-set-key (kbd "C-c e c") 'make-box-comment)
(global-set-key (kbd "C-c e d") 'make-divider)
(global-set-key (kbd "C-c e r") 'make-revision)
(global-set-key (kbd "C-c e g") 'update-file-header)
(global-set-key (kbd "C-c e l") 'prolusion/duplicate-line)
(global-set-key (kbd "C-c e e") 'iedit-mode)
(global-set-key (kbd "C-c e f") 'helm-mini)
(global-set-key (kbd "C-c e b") 'helm-buffers-list)
(global-set-key (kbd "C-c e k") 'helm-kill-ring)

(global-set-key (kbd "C-M-f") 'sp-forward-sexp)
(global-set-key (kbd "C-M-b") 'sp-backward-sexp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'prolusion-editor)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; prolusion-editor.el ends here
