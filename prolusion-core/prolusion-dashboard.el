;; Version: $Id$
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary: See credit at EOF
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dashboard requirements
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(prolusion-require-package 'bookmark)
(prolusion-require-package 'recentf)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dashboard functions, modes and variables
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defface prolusion/dashboard-banner-face  '((t (:height 1.3 :foreground "#bc6ec5" :bold t))) "")
(defface prolusion/dashboard-section-face '((t (:height 1.1 :foreground "#4f97d7" :bold t))) "")

(defun prolusion/dashboard-subseq (seq start end)
  (let ((len (length seq)))
    (cl-subseq seq start (and (number-or-marker-p end) (min len end)))))

(defvar prolusion/dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [tab] 'widget-forward)
    (define-key map [backtab] 'widget-backward)
    (define-key map (kbd "RET") 'widget-button-press)
    (define-key map [down-mouse-1] 'widget-button-click)
    (define-key map (kbd "g") #'prolusion/dashboard-insert-startupify-lists)
    map))

(define-derived-mode prolusion/dashboard-mode special-mode "Dashboard" ""
  :group 'prolusion/dashboard
  :syntax-table nil
  :abbrev-table nil
  (whitespace-mode -1)
  (linum-mode -1)
  (setq inhibit-startup-screen t)
  (setq buffer-read-only t)
  (setq truncate-lines t))

(defgroup prolusion/dashboard nil ""
  :group 'prolusion/dashboard)

(defcustom prolusion/dashboard-page-separator "\n\n" ""
  :type 'string
  :group 'prolusion/dashboard)

(defconst prolusion/dashboard-banner-length 75 "")

(defvar prolusion/dashboard-item-generators '((recents   . prolusion/dashboard-insert-recents)
                                              (bookmarks . prolusion/dashboard-insert-bookmarks)
                                              (projects  . prolusion/dashboard-insert-projects)))

(defvar prolusion/dashboard-items '((recents   . 5)
                                    (bookmarks . 5)
                                    (projects  . 5)) "")

(defvar prolusion/dashboard-items-default-length 20 "")

(defun prolusion/dashboard-get-string-from-file (file) ""
       (with-temp-buffer
         (insert-file-contents file)
         (buffer-string)))

(defun prolusion/dashboard-insert-ascii-banner-centered (file) ""
       (insert
        (with-temp-buffer
          (setq banner (propertize (prolusion/dashboard-get-string-from-file file) 'face 'prolusion/dashboard-banner-face))
          (insert banner)
          (let ((banner-width 0))
            (while (not (eobp))
              (let ((line-length (- (line-end-position) (line-beginning-position))))
                (if (< banner-width line-length)
                    (setq banner-width line-length)))
              (forward-line 1))
            (goto-char 0)
            (let ((margin (max 0 (floor (/ (- prolusion/dashboard-banner-length banner-width) 3)))))
              (while (not (eobp))
                (when (not (looking-at-p "$"))
                  (insert (make-string margin ?\ )))
                (forward-line 1))))
          (buffer-string))))

(defun prolusion/dashboard-insert-banner () ""
       (goto-char (point-max))
       (prolusion/dashboard-insert-ascii-banner-centered
        (expand-file-name "prolusion-dashboard-banner.el" prolusion-core-dir)))

(defun prolusion/dashboard-insert-file-list (list-display-name list) ""
       (setq list-display-name-faced (propertize list-display-name 'face 'prolusion/dashboard-section-face))
       (insert list-display-name-faced)
       (when (car list)
         (mapc (lambda (el)
                 (insert "\n    ")
                 (widget-create 'push-button
                                :action `(lambda (&rest ignore) (find-file-existing ,el))
                                :mouse-face 'highlight
                                :follow-link "\C-m"
                                :button-prefix ""
                                :button-suffix ""
                                :format "%[%t%]"
                                (abbreviate-file-name el)))
               list)))

(defun prolusion/dashboard-insert-project-list (list-display-name list) ""
       (setq list-display-name-faced (propertize list-display-name 'face 'prolusion/dashboard-section-face))
       (insert list-display-name-faced)
       (when (car list)
         (mapc (lambda (el)
                 (insert "\n    ")
                 (widget-create 'push-button
                                :action `(lambda (&rest ignore)
                                           (projectile-switch-project-by-name ,el))
                                :mouse-face 'highlight
                                :follow-link "\C-m"
                                :button-prefix ""
                                :button-suffix ""
                                :format "%[%t%]"
                                (abbreviate-file-name el)))
               list)))

(defun prolusion/dashboard-insert-bookmark-list (list-display-name list) ""
       (setq list-display-name-faced (propertize list-display-name 'face 'prolusion/dashboard-section-face))
       (insert list-display-name-faced)
       (when (car list)
         (mapc (lambda (el)
                 (insert "\n    ")
                 (widget-create 'push-button
                                :action `(lambda (&rest ignore) (bookmark-jump ,el))
                                :mouse-face 'highlight
                                :follow-link "\C-m"
                                :button-prefix ""
                                :button-suffix ""
                                :format "%[%t%]"
                                (format "%s - %s" el (abbreviate-file-name
                                                      (bookmark-get-filename el)))))
               list)))

(defun prolusion/dashboard-insert-page-break () ""
       (prolusion/dashboard-append prolusion/dashboard-page-separator))

(defun prolusion/dashboard-append (msg &optional messagebuf) ""
       (with-current-buffer (get-buffer-create "*dashboard*")
         (goto-char (point-max))
         (let ((buffer-read-only nil))
           (insert msg))))

(defmacro prolusion/dashboard-insert--shortcut (shortcut-char search-label &optional no-next-line) ""
          `(define-key prolusion/dashboard-mode-map ,shortcut-char
             (lambda ()
               (interactive)
               (unless (search-forward ,search-label (point-max) t)
                 (search-backward ,search-label (point-min) t))
               ,@(unless no-next-line
                   '((forward-line 1)))
               (back-to-indentation))))

(defun prolusion/dashboard-goto-link-line () ""
       (interactive)
       (with-current-buffer "*dashboard*"
         (goto-char (point-min))
         (re-search-forward "Homepage")
         (beginning-of-line)
         (widget-forward 1)))

(defun prolusion/dashboard-insert-recents (list-size) ""
       (recentf-mode)
       (when (prolusion/dashboard-insert-file-list
              "Recent Files:"
              (prolusion/dashboard-subseq recentf-list 0 list-size))
         (prolusion/dashboard-insert--shortcut "r" "Recent Files:")))

(defun prolusion/dashboard-insert-bookmarks (list-size) ""
       (require 'bookmark)
       (when (prolusion/dashboard-insert-bookmark-list
              "Bookmarks:"
              (prolusion/dashboard-subseq (bookmark-all-names)
                                0 list-size))
         (prolusion/dashboard-insert--shortcut "m" "Bookmarks:")))

(defun prolusion/dashboard-insert-projects (list-size) ""
       (if (bound-and-true-p projectile-mode)
           (progn
             (projectile-load-known-projects)
             (when (prolusion/dashboard-insert-project-list
                    "Projects:"
                    (prolusion/dashboard-subseq (projectile-relevant-known-projects)
                                      0 list-size))
               (prolusion/dashboard-insert--shortcut "p" "Projects:")))
         (error "Projects list depends on 'projectile-mode` to be activated")))

(defun prolusion/dashboard-insert-startupify-lists () ""
       (interactive)
       (with-current-buffer (get-buffer-create "*dashboard*")
         (let ((buffer-read-only nil)
               (list-separator "\n\n"))
           (erase-buffer)
           (prolusion/dashboard-insert-banner)
           (prolusion/dashboard-insert-page-break)
           (mapc (lambda (els)
                   (let* ((el (or (car-safe els) els))
                          (list-size
                           (or (cdr-safe els)
                               prolusion/dashboard-items-default-length))
                          (item-generator
                           (cdr-safe (assoc el prolusion/dashboard-item-generators))
                           ))
                     (funcall item-generator list-size)
                     (prolusion/dashboard-insert-page-break)
                     ))
                 prolusion/dashboard-items))
         (prolusion/dashboard-mode)
         (goto-char (point-min))))

;;;###autoload

(defun prolusion/dashboard-setup-startup-hook () ""
       (if (< (length command-line-args) 2 )
           (progn
             (add-hook 'after-init-hook (lambda () (prolusion/dashboard-insert-startupify-lists)))
             (add-hook 'emacs-startup-hook
                       '(lambda ()
                          (switch-to-buffer "*dashboard*")
                          (goto-char (point-min))
                          (redisplay))))))

(declare-function projectile-load-known-projects "ext:projectile.el")
(declare-function projectile-relevant-known-projects "ext:projectile.el")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dashboard setup
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(prolusion/dashboard-setup-startup-hook)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'prolusion-dashboard)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dashboard credits
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; dashboard.el --- A startup screen extracted from Spacemacs

;; Copyright (c) 2016 Rakan Al-Hneiti & Contributors
;;
;; Author: Rakan Al-Hneiti
;; URL: https://github.com/rakanalh/emacs-dashboard

;; A shameless extraction of Spacemacs’ startup screen, with sections for
;; bookmarks, projectile projects and more.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; prolusion-dashboard.el ends here
