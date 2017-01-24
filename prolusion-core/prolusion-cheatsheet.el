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

(require 'cl-lib)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cheatsheet faces
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defface prolusion//cheatsheet-group-face '((t :foreground "red")) "Group name font face.")

(defface prolusion//cheatsheet-key-face '((t :foreground "orange")) "Cheat key font face.")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cheatsheet variables
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar prolusion//cheatsheet-cheat-list '() "List of cheats.")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cheatsheet getter functions
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prolusion//cheatsheet-if-symbol-to-string (string-like)
  "Convert STRING-LIKE to string."
  (if (symbolp string-like) (symbol-name string-like) string-like))

(defun prolusion//cheatsheet-group-name (group)
  "Get GROUP name."
  (prolusion//cheatsheet-if-symbol-to-string (plist-get group :name)))

(defun prolusion//cheatsheet-group-cheats (group)
  "Get GROUP cheats."
  (prolusion//cheatsheet-if-symbol-to-string (plist-get group :cheats)))

(defun prolusion//cheatsheet-cheat-key (cheat)
  "Get CHEAT key."
  (prolusion//cheatsheet-if-symbol-to-string (plist-get cheat :key)))

(defun prolusion//cheatsheet-cheat-group (cheat)
  "Get CHEAT group."
  (prolusion//cheatsheet-if-symbol-to-string (plist-get cheat :group)))

(defun prolusion//cheatsheet-cheat-description (cheat)
  "Get CHEAT description."
  (prolusion//cheatsheet-if-symbol-to-string (plist-get cheat :description)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cheatsheet convenience functions
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prolusion//cheatsheet-cheat-groups ()
  "Get all groups, submitted to cheatsheet."
  (reverse (delete-dups
            (mapcar 'prolusion//cheatsheet-cheat-group
                    prolusion//cheatsheet-cheat-list))))

(defun prolusion//cheatsheet-get-group (group)
  "Get group struct with all cheats, belonging to GROUP."
  (cl-flet ((is-current-group (cheat)
                              (if (string= (prolusion//cheatsheet-cheat-group cheat)
                                           group)
                                  cheat
                                nil)))
    (delq nil (mapcar #'is-current-group prolusion//cheatsheet-cheat-list))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cheatsheet formatting functions
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prolusion//cheatsheet-format-cheat (cheat key-cell-length)
  "Format CHEAT row with KEY-CELL-LENGTH key cell length."
  (let* ((format-string (format "%%%ds - %%s\n" key-cell-length))
         (key (prolusion//cheatsheet-cheat-key cheat))
         (description (prolusion//cheatsheet-cheat-description cheat))
         (faced-key (propertize key 'face 'prolusion//cheatsheet-key-face)))
    (format format-string faced-key description)))

(defun prolusion//cheatsheet-format-group (group)
  "Format GROUP to table."
  (cl-flet ((key-length (cheat) (length (prolusion//cheatsheet-cheat-key cheat)))
            (format-cheat (key-cell-length cheat)
                          (prolusion//cheatsheet-format-cheat cheat key-cell-length)))

    (let* ((name (prolusion//cheatsheet-group-name group))
           (cheats (prolusion//cheatsheet-group-cheats group))
           (key-max-length (apply 'max (mapcar #'key-length cheats)))
           (key-cell-length (+ 2 key-max-length))
           (format-cheat (apply-partially #'format-cheat key-cell-length))
           (formatted-cheats (apply 'concat (mapcar format-cheat cheats)))
           (faced-group-name (propertize name 'face 'prolusion//cheatsheet-group-face)))
      (concat faced-group-name "\n" formatted-cheats "\n"))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cheatsheet setup
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cheatsheet autoloads
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload

(defun prolusion/cheatsheet-format ()
  "Print the whole cheatsheet."
  (let* ((cheatsheet (prolusion/cheatsheet-get))
         (formatted-groups (mapcar 'prolusion//cheatsheet-format-group cheatsheet))
         (formatted-cheatsheet (apply 'concat formatted-groups)))
    formatted-cheatsheet))

;;;###autoload

(defun prolusion/cheatsheet-add (&rest cheat)
  "Add CHEAT to cheatsheet."
  (add-to-list 'prolusion//cheatsheet-cheat-list cheat))

(defun prolusion/cheatsheet-get ()
  "Get cheatsheet as list of group structs, keeping defining order."
  (cl-flet ((make-group (group)
                        (list :name group
                              :cheats (prolusion/cheatsheet-get-group group))))
    (mapcar #'make-group (prolusion/cheatsheet-cheat-groups))))

(defun prolusion/cheatsheet-add-group (group &rest cheats)
  (mapcar '(lambda (cheat) (append `(:group ,group) cheat)) cheats))

;;;###autoload

(define-derived-mode prolusion/cheatsheet-mode fundamental-mode "Cheat Sheet"
  "Set major mode for viewing cheat sheets.")

(define-key prolusion/cheatsheet-mode-map (kbd "C-q") 'kill-buffer-and-window)

(defun prolusion/cheatsheet-show ()
  "Create buffer and show cheatsheet."
  (interactive)
  (switch-to-buffer-other-window "*cheatsheet*")
  (prolusion/cheatsheet-mode)
  (erase-buffer)
  (insert (prolusion/cheatsheet-format))
  (setq buffer-read-only t))

(provide 'prolusion-cheatsheet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; prolusion-cheatsheet.el ends here

;; Copyright (C) 2015 Shirin Nikita and contributors
;;
;; Author: Shirin Nikita <shirin.nikita@gmail.com> and contributors
;; URL: http://github.com/darksmile/cheatsheet/
;; Package-Requires: ((emacs "24") (cl-lib "0.5"))
;; Version: 1.0
;; Keywords: convenience, usability
;;
;; This file is not part of GNU Emacs
;;
;;; Licence: Licensed under the same terms as Emacs.
;;
;;; Commentary:
;;
;; (cheatsheet-add :group 'Common
;;                 :key "C-x C-c"
;;                 :description "leave Emacs.")
