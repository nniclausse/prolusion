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

(require 'bookmark)
(require 'recentf)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dashboard functions, modes and variables
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst dashboard-mode-font-lock-keywords
  (list (cons "Recent Files:" 'font-lock-type-face)
        (cons "Bookmarks:"    'font-lock-type-face)
        (cons "Projects:"     'font-lock-type-face)
        (cons ""             'markdown-header-face-1)))

(defun dashboard-subseq (seq start end)
  (let ((len (length seq)))
    (cl-subseq seq start (and (number-or-marker-p end) (min len end)))))

(defvar dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [tab] 'widget-forward)
    (define-key map [backtab] 'widget-backward)
    (define-key map (kbd "RET") 'widget-button-press)
    (define-key map [down-mouse-1] 'widget-button-click)
    (define-key map (kbd "g") #'dashboard-insert-startupify-lists)
    map))

(define-derived-mode dashboard-mode special-mode "Dashboard" ""
  :group 'dashboard
  :syntax-table nil
  :abbrev-table nil
  (whitespace-mode -1)
  (linum-mode -1)
  (setq inhibit-startup-screen t)
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (set (make-local-variable 'font-lock-defaults) '(dashboard-mode-font-lock-keywords)))

(defgroup dashboard nil ""
  :group 'dashboard)

(defcustom dashboard-page-separator "\n\n" ""
  :type 'string
  :group 'dashboard)

(defconst dashboard-banner-length 75 "")

(defvar dashboard-item-generators '((recents   . dashboard-insert-recents)
                                    (bookmarks . dashboard-insert-bookmarks)
                                    (projects  . dashboard-insert-projects)))

(defvar dashboard-items '((recents   . 5)
                          (bookmarks . 5)
                          (projects  . 5)) "")

(defvar dashboard-items-default-length 20 "")

(defun dashboard-insert-ascii-banner-centered (file) ""
       (insert
        (with-temp-buffer
          (insert-file-contents file)
          (let ((banner-width 0))
            (while (not (eobp))
              (let ((line-length (- (line-end-position) (line-beginning-position))))
                (if (< banner-width line-length)
                    (setq banner-width line-length)))
              (forward-line 1))
            (goto-char 0)
            (let ((margin (max 0 (floor (/ (- dashboard-banner-length banner-width) 2)))))
              (while (not (eobp))
                (when (not (looking-at-p "$"))
                  (insert (make-string margin ?\ )))
                (forward-line 1))))
          (buffer-string))))

(defun dashboard-insert-banner () ""
       (goto-char (point-max))
       (dashboard-insert-ascii-banner-centered
        (expand-file-name "prolusion-dashboard.txt" prolusion-core-dir)))

(defun dashboard-insert-file-list (list-display-name list) ""
       (when (car list)
         (insert list-display-name)
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

(defun dashboard-insert-project-list (list-display-name list) ""
       (when (car list)
         (insert list-display-name)
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

(defun dashboard-insert-bookmark-list (list-display-name list) ""
       (when (car list)
         (insert list-display-name)
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

(defun dashboard-insert-page-break () ""
       (dashboard-append dashboard-page-separator))

(defun dashboard-append (msg &optional messagebuf) ""
       (with-current-buffer (get-buffer-create "*dashboard*")
         (goto-char (point-max))
         (let ((buffer-read-only nil))
           (insert msg))))

(defmacro dashboard-insert--shortcut (shortcut-char search-label &optional no-next-line) ""
          `(define-key dashboard-mode-map ,shortcut-char
             (lambda ()
               (interactive)
               (unless (search-forward ,search-label (point-max) t)
                 (search-backward ,search-label (point-min) t))
               ,@(unless no-next-line
                   '((forward-line 1)))
               (back-to-indentation))))

(defun dashboard-goto-link-line () ""
       (interactive)
       (with-current-buffer "*dashboard*"
         (goto-char (point-min))
         (re-search-forward "Homepage")
         (beginning-of-line)
         (widget-forward 1)))

(defun dashboard-insert-recents (list-size) ""
       (recentf-mode)
       (when (dashboard-insert-file-list
              "Recent Files:"
              (dashboard-subseq recentf-list 0 list-size))
         (dashboard-insert--shortcut "r" "Recent Files:")))

(defun dashboard-insert-bookmarks (list-size) ""
       (require 'bookmark)
       (when (dashboard-insert-bookmark-list
              "Bookmarks:"
              (dashboard-subseq (bookmark-all-names)
                                0 list-size))
         (dashboard-insert--shortcut "m" "Bookmarks:")))

(defun dashboard-insert-projects (list-size) ""
       (if (bound-and-true-p projectile-mode)
           (progn
             (projectile-load-known-projects)
             (when (dashboard-insert-project-list
                    "Projects:"
                    (dashboard-subseq (projectile-relevant-known-projects)
                                      0 list-size))
               (dashboard-insert--shortcut "p" "Projects:")))
         (error "Projects list depends on 'projectile-mode` to be activated")))

(defun dashboard-insert-startupify-lists () ""
       (interactive)
       (with-current-buffer (get-buffer-create "*dashboard*")
         (let ((buffer-read-only nil)
               (list-separator "\n\n"))
           (erase-buffer)
           (dashboard-insert-banner)
           (dashboard-insert-page-break)
           (mapc (lambda (els)
                   (let* ((el (or (car-safe els) els))
                          (list-size
                           (or (cdr-safe els)
                               dashboard-items-default-length))
                          (item-generator
                           (cdr-safe (assoc el dashboard-item-generators))
                           ))
                     (funcall item-generator list-size)
                     (dashboard-insert-page-break)
                     ))
                 dashboard-items))
         (dashboard-mode)
         (goto-char (point-min))))

;;;###autoload

(defun dashboard-setup-startup-hook () ""
       (if (< (length command-line-args) 2 )
           (progn
             (add-hook 'after-init-hook (lambda () (dashboard-insert-startupify-lists)))
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

(dashboard-setup-startup-hook)

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
