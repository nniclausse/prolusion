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
;; Behavior requirements
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(prolusion/require-package 'anzu)
(prolusion/require-package 'beacon)
(prolusion/require-package 'recentf)
(prolusion/require-package 'diminish)
(prolusion/require-package 'saveplace)
(prolusion/require-package 'which-key)
(prolusion/require-package 'resize-window)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Behavior setup
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Encoding system
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(prefer-coding-system 'utf-8)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)

;; Frame title
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq frame-title-format
  '("Emacs - "
    (:eval
     (if (buffer-file-name)
         (abbreviate-file-name (buffer-file-name)) "%b"))))

;; yes or no
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fset 'yes-or-no-p 'y-or-n-p)

;; custom file
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq custom-file (expand-file-name "prolusion-behavior-custom" prolusion-save-dir))

;; save place
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq save-place-file (expand-file-name "prolusion-behavior-place" prolusion-save-dir))

(setq-default save-place t)

;; minimize buffer count
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-default display-buffer-reuse-frames t)

;; save history
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq savehist-additional-variables
 '(search ring regexp-search-ring)
   savehist-autosave-interval 60
   savehist-file (expand-file-name "prolusion-behavior-history" prolusion-save-dir))

(savehist-mode +1)

;; recent files
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq recentf-save-file (expand-file-name "prolusion-behavior-files" prolusion-save-dir))
(setq recentf-keep '(file-remote-p file-readable-p))
(setq recentf-max-saved-items 500)
(setq recentf-max-menu-items 15)
(setq recentf-auto-cleanup 'never)

(recentf-mode +1)

;; automatic save
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq auto-save-list-file-prefix prolusion-save-dir)

(setq backup-directory-alist
      `((".*" . ,prolusion-save-dir)))
(setq auto-save-file-name-transforms
      `((".*"   ,prolusion-save-dir t)))

(global-auto-revert-mode t)

;; keybindings helper
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq which-key-idle-delay 0.2)

(which-key-mode)

;; dired
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies  'always)

;; highlight cursor after window moves
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(beacon-mode 1)

;; show number of candidates for searches
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-anzu-mode +1)

(setq anzu-cons-mode-line-p nil)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Behavior modeline
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(diminish        'anzu-mode)
(diminish      'abbrev-mode)
(diminish      'beacon-mode)
(diminish   'which-key-mode)
(diminish 'auto-revert-mode)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Behavior keybindings
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq text-scale-mode-step 1.05)

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-x O")
   (lambda ()
     (interactive)
     (other-window -1)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'prolusion-behavior)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; prolusion-behavior.el ends here
