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
(prolusion/require-package 'highlight-indentation)

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
  (setq doom-themes-enable-bold t)
  (setq doom-themes-enable-italic t)
  (setq doom-one-brighter-modeline nil)
  (setq doom-one-brighter-comments t)
  (setq doom-neotree-file-icons t)
  (setq ns-use-srgb-colorspace t)

  (load-theme 'doom-one t)

  (doom-themes-nlinum-config)
  (doom-themes-neotree-config)

  (setq nlinum-format "%d ")

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
  (setq spaceline-all-the-icons-slim-render nil)
  (setq spaceline-all-the-icons-icon-set-git-ahead (quote commit))
  (setq spaceline-all-the-icons-icon-set-window-numbering (quote square))
  (setq spaceline-all-the-icons-highlight-file-name t)
  (setq spaceline-all-the-icons-separator-type (quote none))
  (setq spaceline-all-the-icons-clock-always-visible nil)
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

(defvar prolusion--track-mouse nil)

(defun prolusion//track-mouse-select-window (event)
  ""
  (interactive "e")
  (prog1 (if prolusion--track-mouse
             (let ((current-window (get-buffer-window (current-buffer)))
                   (event-window (posn-window (event-start event))))
               (if (and (or (not (window-minibuffer-p current-window))
                            (not (minibuffer-window-active-p current-window)))
                        (windowp event-window)
                        (or (not (window-minibuffer-p event-window))
                            (minibuffer-window-active-p event-window)))
                   (progn
                     (or (eq (window-buffer current-window)
                             (window-buffer event-window))
                         (run-hooks 'mouse-leave-buffer-hook))
                     (if (mouse-select-window event)
                       (select-window event-window))))))
    (setq unread-command-events
          (nconc unread-command-events (list event)))))

(defun prolusion/toggle-mouse-tracking (&optional arg verbose)
  ""
  (interactive (list current-prefix-arg t))
  (if (or (null arg)
          (if (> (prefix-numeric-value arg) 0)
              (not prolusion--track-mouse)
            prolusion--track-mouse))
      (progn
        (cond ((setq prolusion--track-mouse (not prolusion--track-mouse))
               (put 'prolusion--track-mouse 'track-mouse track-mouse)
               (setq track-mouse t)
               (put 'prolusion--track-mouse 'mouse-movement
                    (lookup-key special-event-map [mouse-movement]))
               (define-key special-event-map [mouse-movement]
                 'prolusion//track-mouse-select-window))
              (t
               (setq track-mouse (get 'prolusion--track-mouse 'track-mouse))
               (define-key special-event-map [mouse-movement]
                 (get 'prolusion--track-mouse 'mouse-movement))))
        (if (or (interactive-p) verbose)
            (message "Track mouse is %s"
                     (if prolusion--track-mouse "enabled" "disabled"))))
    (if (or (interactive-p) verbose)
        (message "Track mouse is already %s"
                 (if prolusion--track-mouse "enabled" "disabled"))))
  prolusion--track-mouse)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'prolusion-ui)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; prolusion-ui.el ends here
