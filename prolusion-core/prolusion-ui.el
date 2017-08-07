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

(prolusion/require-package 'solaire-mode)
(prolusion/require-package 'rainbow-mode)
(prolusion/require-package 'font-lock+)
(prolusion/require-package 'all-the-icons)
(prolusion/require-package 'all-the-icons-dired)
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
    (set-frame-font "Source Code Pro-12" nil t)
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
  (setq all-the-icons-color-icons nil)

  (setq doom-themes-enable-bold t)
  (setq doom-themes-enable-italic t)
  (setq doom-one-brighter-modeline nil)
  (setq doom-one-brighter-comments t)
  (setq doom-neotree-file-icons t)
  (setq ns-use-srgb-colorspace t)

  (load-theme 'doom-one t)

  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)

  (setq nlinum-format "%d ")
  (setq nlinum-highlight-current-line t)

  (add-hook 'after-change-major-mode-hook #'turn-on-solaire-mode)
  (add-hook 'after-revert-hook #'turn-on-solaire-mode)
  (add-hook 'minibuffer-setup-hook #'solaire-mode-in-minibuffer)
  (add-hook 'ediff-prepare-buffer-hook #'solaire-mode)

  (require 'spaceline-config)

  (spaceline-helm-mode)
  (spaceline-info-mode))

(setq inhibit-startup-message t)

(use-package spaceline-all-the-icons
  :after spaceline
  :config
  (setq powerline-text-scale-factor 0.9)
  (setq spaceline-all-the-icons-slim-render nil)
  (setq spaceline-all-the-icons-icon-set-git-ahead (quote commit))
  (setq spaceline-all-the-icons-icon-set-window-numbering (quote square))
  (setq spaceline-all-the-icons-highlight-file-name t)
  (setq spaceline-all-the-icons-primary-separator "")
  (setq spaceline-all-the-icons-separator-type (quote cup))
  (setq spaceline-all-the-icons-slim-render nil)
  (setq spaceline-responsive nil)
  (spaceline-all-the-icons-theme)
  (spaceline-all-the-icons--setup-anzu)
  (spaceline-all-the-icons--setup-package-updates)
  (spaceline-all-the-icons--setup-paradox)
  (spaceline-all-the-icons--setup-neotree)
  (spaceline-toggle-all-the-icons-buffer-path-off)
  (spaceline-toggle-all-the-icons-buffer-position-on)
  (spaceline-toggle-all-the-icons-hud-on)
  (spaceline-toggle-all-the-icons-fullscreen-on)
  (spaceline-toggle-all-the-icons-flycheck-status-on)
  (spaceline-toggle-all-the-icons-vc-status-on)
  (spaceline-toggle-all-the-icons-git-status-on)
  (spaceline-toggle-all-the-icons-vc-icon-on)
  (spaceline-toggle-all-the-icons-mode-icon-on)
  (spaceline-toggle-all-the-icons-package-updates-on)
  (spaceline-toggle-all-the-icons-text-scale-on)
  (spaceline-toggle-all-the-icons-region-info-on)
  :when (display-graphic-p))

(use-package highlight-indentation
  :commands (highlight-indentation-mode highlight-indentation-current-column-mode)
  :config
  (defun prolusion//inject-trailing-whitespace (&optional start end)
    (unless indent-tabs-mode
      (save-match-data
        (save-excursion
          (let ((end-marker (copy-marker (or end (point-max))))
                (start (or start (point-min))))
            (goto-char start)
            (while (and (re-search-forward "^$" end-marker t) (< (point) end-marker))
              (let (line-start line-end next-start next-end)
                (save-excursion
                  (forward-line -1)
                  (setq line-start (point)
                        line-end (save-excursion (back-to-indentation) (point)))
                  (forward-line 2)
                  (setq next-start (point)
                        next-end (save-excursion (back-to-indentation) (point)))
                  (forward-line -1)
                  (let* ((line-indent (- line-end line-start))
                         (next-indent (- next-end next-start))
                         (indent (min line-indent next-indent)))
                    (insert (make-string (if (zerop indent) 0 (1+ indent)) ? )))))
              (forward-line 1)))))
      (set-buffer-modified-p nil))
    nil)

  (defun prolusion//highlight-indentation-handle-whitespace ()
    (if (or highlight-indentation-mode highlight-indentation-current-column-mode)
        (progn
          (prolusion//inject-trailing-whitespace)
          (add-hook 'before-save-hook #'delete-trailing-whitespace nil t)
          (add-hook 'after-save-hook #'prolusion//inject-trailing-whitespace nil t))
      (remove-hook 'before-save-hook #'delete-trailing-whitespace t)
      (remove-hook 'after-save-hook #'prolusion//inject-trailing-whitespace t)
      (delete-trailing-whitespace)))

  (add-hook 'highlight-indentation-mode-hook 'prolusion//highlight-indentation-handle-whitespace)
  (add-hook 'highlight-indentation-current-column-mode-hook 'prolusion//highlight-indentation-handle-whitespace)

  :when (display-graphic-p))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UI functions
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar prolusion--doom-themes-track-mouse nil)

(defun prolusion//doom-themes-neotree-handle-callback (event)
  ""
  (interactive "e")
  (let ((p_x_w (car (posn-x-y (event-start event)))))
    (if (and (< p_x_w 10)) ;; (not (get-buffer " *NeoTree*")))
        (neotree-toggle)))
  (setq unread-command-events (nconc unread-command-events (list event))))

(defun prolusion/doom-themes-neotree-handle-config ()
  ""
  (interactive)
  (cond ((setq prolusion--doom-themes-track-mouse (not prolusion--doom-themes-track-mouse))
         (put 'prolusion--doom-themes-track-mouse 'track-mouse track-mouse)
         (setq track-mouse t)
         (put 'prolusion--doom-themes-track-mouse 'mouse-movement
              (lookup-key special-event-map [mouse-movement]))
         (define-key special-event-map [mouse-movement]
           'prolusion//doom-themes-neotree-handle-callback))
        (t
         (setq track-mouse (get 'prolusion--doom-themes-track-mouse 'track-mouse))
         (define-key special-event-map [mouse-movement]
           (get 'prolusion--doom-themes-track-mouse 'mouse-movement))))
  (message "Prolusion doom themes mouse tracking is %s"
           (if prolusion--doom-themes-track-mouse "enabled" "disabled"))
  prolusion--doom-themes-track-mouse)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UI hooks
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UI keybindings
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key [f7] 'nlinum-mode)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UI modeline
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(diminish 'rainbow-mode)
(diminish 'solaire-mode)
(diminish 'page-break-lines-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'prolusion-ui)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; prolusion-ui.el ends here
