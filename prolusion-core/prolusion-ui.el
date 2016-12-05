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
;; UI requirements
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(prolusion-require-package 'spaceline)
(prolusion-install-package 'spacemacs-theme)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UI setup
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq initial-frame-alist '((width . 75) (height . 40)))

(set-frame-font "Source Code Pro-13" nil t)

(tooltip-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)

(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

(fringe-mode '(8 . 0))

(when (display-graphic-p)
  (load-theme 'spacemacs-dark t)
  (setq ns-use-srgb-colorspace nil)
  (require 'spaceline-config)
  (spaceline-define-segment persp-name
    (when (bound-and-true-p persp-mode)
      (fboundp 'safe-persp-name)
      (fboundp 'get-frame-persp)
      (or (not (string= persp-nil-name (safe-persp-name (get-frame-persp))))
          spaceline-display-default-perspective)
      (let ((name (safe-persp-name (get-frame-persp))))
        (propertize
         (if (file-directory-p name)
             (file-name-nondirectory (directory-file-name name))
           name)
         'face 'bold))))
  (setq powerline-default-separator 'wave)
  (setq spaceline-display-default-perspective t)
  (setq spaceline-toggle-window-number-on-p t)
  (spaceline-spacemacs-theme)
  (spaceline-helm-mode +1))

(when (display-graphic-p)
  (set-frame-parameter (selected-frame) 'alpha '(98 95))
  (add-to-list 'default-frame-alist    '(alpha   98 95)))

(setq inhibit-startup-message t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'prolusion-ui)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; prolusion-ui.el ends here
