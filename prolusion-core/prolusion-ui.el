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
(prolusion/require-package 'fontawesome)
(prolusion/require-package 'octicons)
(prolusion/require-package 'all-the-icons)
(prolusion/require-package 'page-break-lines)
(prolusion/require-package 'spaceline)
(prolusion/install-package 'spacemacs-theme)
(prolusion/require-package 'info+)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UI setup
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq initial-frame-alist '((width . 75) (height . 50)))

(set-frame-font "Source Code Pro-9" nil t)

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

(defvar prolusion-upgrade-count nil)

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
               (act1 . "#080808")
               (vertical-border . "#100a14")))))
        (load-theme 'spacemacs-dark t))
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
  (spaceline-define-segment prolusion-narrow
    (when (buffer-narrowed-p)
      "Narrowed"))
  (spaceline-define-segment prolusion-conda-environment
    (when (string= major-mode "python-mode")
      (if (not (equal (length conda-env-current-name) 0))
          (propertize (concat "conda: " conda-env-current-name) 'face '((t (:foreground "IndianRed"))))
        (propertize "no conda environment" 'face '((t (:foreground "IndianRed")))))))
  (spaceline-define-segment prolusion-upgrades-count
    (when (string= major-mode "prolusion-dashboard-mode")
      (unless prolusion-upgrade-count
        (save-window-excursion
          (package-list-packages)
          (package-menu-mode)
          (setq prolusion-upgrade-count (length (package-menu--find-upgrades)))
          (kill-buffer (get-buffer "*Packages*"))))
      (if (> prolusion-upgrade-count 0)
          (propertize (format "%s %d" (propertize "" 'face '((t (:family "FontAwesome")))) prolusion-upgrade-count) 'face '((t (:foreground "MediumSeaGreen")))))))
  (setq powerline-default-separator 'wave)
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-modified)
  (setq spaceline-display-default-perspective t)
  (setq spaceline-toggle-window-number-on-p t)
  (setq all-the-icons-scale-factor 1.0)
  (spaceline-spacemacs-theme 'prolusion-narrow 'prolusion-conda-environment 'prolusion-upgrades-count)
  (spaceline-toggle-major-mode-off)
  (spaceline-helm-mode +1)
  (spaceline-info-mode +1))

(when (display-graphic-p)
  (set-frame-parameter (selected-frame) 'alpha '(98 95))
  (add-to-list 'default-frame-alist    '(alpha   98 95)))

(setq inhibit-startup-message t)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UI function
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prolusion/octicons--propertize (glyph)
  ""
  (propertize glyph 'face '(:family "Octicons" :height 1.0)))

(defun prolusion/octicons--source (octicons-alist)
  ""
  (helm-build-sync-source "Select Octicon Icon: "
    :candidates (mapcar (lambda (octicon)
                          (cons (concat (car octicon)
                                        " -> "
                                        (prolusion/octicons--propertize
                                         (cdr octicon)))
                                (cdr octicon)))
                        octicons-alist)
    :action (lambda (candidate)
              (insert (prolusion/octicons--propertize candidate)))
    :candidate-number-limit 999))

;;;###autoload

(defun helm-octicons ()
  (interactive)
  (helm :sources (prolusion/octicons--source octicons-alist)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UI modeline
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(diminish 'rainbow-mode)
(diminish 'page-break-lines-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'prolusion-ui)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; prolusion-ui.el ends here
