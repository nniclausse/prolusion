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

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mail requirements
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (and
       (require 'mu4e nil 'noerror)
       (executable-find "mu")
       (executable-find "msmtp")
       (executable-find "mbsync"))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mail setup
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (setq user-mail-address "julien.wintz@me.com")
  (setq user-full-name "Julien Wintz")

  (setq mu4e-maildir (expand-file-name "~/Mail"))
  (setq mu4e-drafts-folder "/Drafts")
  (setq mu4e-sent-folder   "/Sent")
  (setq mu4e-trash-folder  "/Trash")
  (setq mu4e-attachment-dir "~/Downloads")
  (setq mu4e-sent-messages-behavior 'delete)
  (setq mu4e-get-mail-command "mbsync -a" mu4e-update-interval 300 mu4e-hide-index-messages t)
  (setq message-kill-buffer-on-exit t)
  (setq message-send-mail-function 'message-send-mail-with-sendmail)
  (setq sendmail-program "msmtp")
  (setq message-sendmail-extra-arguments '("--read-envelope-from"))
  (setq message-sendmail-f-is-evil 't)
  (setq mu4e-view-show-addresses t)
  (setq mu4e-compose-dont-reply-to-self t)
  (setq mu4e-change-filenames-when-moving t)
  (setq mu4e-view-show-images t)
  (setq mu4e-index-cleanup nil)
  (setq mu4e-index-lazy-check t)
  (setq mu4e-maildir-shortcuts
        '(("/Inria/Inbox" .  ?i)
          ("/Gmail/Inbox" .  ?g)
          ("/iCloud/Inbox" . ?c)))

  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))

  (defalias 'mail 'mu4e))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package mu4e-alert
  :ensure t
  :config
  (mu4e-alert-enable-notifications)
  (mu4e-alert-set-default-style 'notifier)
  (setq mu4e-alert-interesting-mail-query
        (concat "(maildir:<fu> AND date:today..now"
                " OR maildir:<bar> AND date:today..now"
                " AND flag:unread"))

  (alert-add-rule
   :category "mu4e-alert"
   :predicate (lambda (_) (string-match-p "^mu4e-" (symbol-name major-mode)))
   :continue t)

  (add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
  (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display))



;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'prolusion-mail)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; prolusion-mail.el ends here
