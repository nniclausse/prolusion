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

(prolusion/require-package 'rainbow-mode)
(prolusion/require-package 'font-lock+)
(prolusion/require-package 'all-the-icons)
(prolusion/require-package 'page-break-lines)
(prolusion/require-package 'spaceline)
(prolusion/install-package 'spacemacs-theme)
(prolusion/require-package 'info+)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UI setup
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (eq system-type 'darwin)
    (set-frame-font "Source Code Pro-13" nil t)
  (set-frame-font "Source Code Pro-10" nil t))

(tooltip-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)

(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

(fringe-mode '(8 . 0))

(rainbow-mode 1)

(global-page-break-lines-mode)

(defvar prolusion--upgrades nil)

(defun prolusion//count-upgrades ()
  ""
  (let ((buf (current-buffer)))
    (package-list-packages)
    (with-current-buffer "*Packages*"
      (setq prolusion--upgrades (length (package-menu--find-upgrades))))
    (kill-buffer "*Packages*")
    (switch-to-buffer buf)))

(advice-add 'package-menu-execute :after 'prolusion//count-upgrades)

(custom-set-faces
 '(mu4e-unread-face ((t (:foreground "green" :background nil :inherit nil))))
 '(mu4e-trashed-face ((t (:foreground "dark red" :background nil :inherit nil))))
 '(mu4e-header-highlight-face ((t (:underline nil :background nil :inherit hl-line)))))

(when (display-graphic-p)
  (if prolusion-dark-variant
      (progn
        (unless prolusion-green-variant
          (custom-set-variables
           '(spacemacs-theme-custom-colors
             '((bg1  . "#111111")
               (act1 . "#080808")))))
          (load-theme 'spacemacs-dark t)
          (custom-theme-set-faces 'spacemacs-dark `(vertical-border ((t (:foreground "#000000"))))))
    (load-theme 'spacemacs-light t))
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
  (spaceline-define-segment prolusion-mode-icon
    (let ((icon (all-the-icons-icon-for-buffer)))
      (unless (symbolp icon)
        (propertize icon
                    'help-echo (format "Major-mode: `%s`" major-mode)
                    'display '(raise 0.0)
                    'face `(:height 0.9 :family ,(all-the-icons-icon-family-for-buffer) :inherit)))))
  (spaceline-define-segment prolusion-narrow
    (when (buffer-narrowed-p)
      "Narrowed"))
  (spaceline-define-segment prolusion-conda-environment
    (when (string= major-mode "python-mode")
      (if (not (equal (length conda-env-current-name) 0))
          (propertize (concat "conda: " conda-env-current-name) 'face '(:foreground "IndianRed" :inherit))
        (propertize "no conda environment" 'face '(:foreground "IndianRed" :inherit)))))
  (spaceline-define-segment prolusion-upgrades-count
    (let ((num (or prolusion--upgrades (prolusion//count-upgrades))))
      (propertize
       (concat
        (propertize (all-the-icons-octicon "package" :v-adjust 0.1)
                    'face `(:family ,(all-the-icons-octicon-family) :height 1.0 :inherit))
        (propertize (format " %d" num) 'face `(:height 0.9 :inherit)))
       'help-echo "Open Packages Menu"
       'local-map (make-mode-line-mouse-map
                   'mouse-1 (lambda () (interactive) (package-list-packages)))))
    :when (and active (> (or prolusion--upgrades (prolusion//count-upgrades)) 0)))
  (setq powerline-default-separator 'wave)
  (setq spaceline-display-default-perspective t)
  (setq spaceline-toggle-window-number-on-p t)
  (setq all-the-icons-scale-factor 1.0)
  (spaceline-spacemacs-theme 'prolusion-mode-icon 'prolusion-narrow 'prolusion-conda-environment 'prolusion-upgrades-count)
  (spaceline-helm-mode +1)
  (spaceline-info-mode +1))

;; (when (display-graphic-p)
;;   (set-frame-parameter (selected-frame) 'alpha '(98 95))
;;   (add-to-list 'default-frame-alist    '(alpha   98 95)))

(setq inhibit-startup-message t)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UI modeline
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(diminish 'rainbow-mode)
(diminish 'page-break-lines-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'prolusion-ui)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; prolusion-ui.el ends here
