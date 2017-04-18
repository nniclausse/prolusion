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
(prolusion/require-package 'nlinum)
(prolusion/require-package 'spaceline)
(prolusion/require-package 'spaceline-all-the-icons)
(prolusion/require-package 'doom-themes)
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

(when (display-graphic-p)
  (setq doom-enable-bold t)
  (setq doom-enable-italic t)
  (setq doom-one-brighter-modeline nil)
  (setq doom-one-brighter-comments t)
  (setq ns-use-srgb-colorspace t)

  (load-theme 'doom-one t)

  (require 'doom-neotree)
  (require 'doom-nlinum)

  (add-hook 'find-file-hook 'doom-buffer-mode-maybe)
  (add-hook 'after-revert-hook 'doom-buffer-mode-maybe)
  (add-hook 'ediff-prepare-buffer-hook 'doom-buffer-mode)
  (add-hook 'minibuffer-setup-hook 'doom-brighten-minibuffer)

  (require 'spaceline-config)

  (spaceline-helm-mode +1)
  (spaceline-info-mode +1))

(setq inhibit-startup-message t)

(use-package spaceline-all-the-icons
  :after spaceline
  :config
  (spaceline-all-the-icons-theme)
  (spaceline-all-the-icons--setup-anzu)
  (spaceline-all-the-icons--setup-package-updates)
  (spaceline-all-the-icons--setup-paradox)
  (spaceline-all-the-icons--setup-neotree)
  (setq spaceline-all-the-icons-flycheck-alternate nil)
  (setq spaceline-all-the-icons-highlight-file-name t)
  (setq spaceline-all-the-icons-separator-type (quote none)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UI modeline
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(diminish 'rainbow-mode)
(diminish 'page-break-lines-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'prolusion-ui)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; prolusion-ui.el ends here
