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

(defface prolusion//cheatsheet-group-face '((t :foreground "#4f97d7" :bold t)) "Group name font face.")

(defface prolusion//cheatsheet-key-face '((t :foreground "#bc6ec5" :bold t)) "Cheat key font face.")

(defface prolusion//cheatsheet-separator-face '((t :foreground "#3fa589")) "Cheat separator font face.")

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
  (let* ((format-string (format "%%%ds %%s %%s\n" key-cell-length))
         (key (prolusion//cheatsheet-cheat-key cheat))
         (description (prolusion//cheatsheet-cheat-description cheat))
         (faced-key (propertize key 'face 'prolusion//cheatsheet-key-face))
         (faced-sep (propertize "→" 'face 'prolusion//cheatsheet-separator-face)))
    (format format-string faced-key faced-sep description)))

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
           (faced-group-name (propertize (concat name ":") 'face 'prolusion//cheatsheet-group-face)))
      (concat "\f\n" faced-group-name "\n\n" formatted-cheats "\n"))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cheatsheet minor mode
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-derived-mode prolusion/cheatsheet-mode fundamental-mode "CheatSheet"
  "Set major mode for viewing cheat sheets."
  (prettify-symbols-mode +1)
  (page-break-lines-mode +1)
  (setq truncate-lines t))

(define-key prolusion/cheatsheet-mode-map (kbd "q") 'kill-buffer-and-window)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cheatsheet autoload functions
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
                              :cheats (prolusion//cheatsheet-get-group group))))
    (mapcar #'make-group (prolusion//cheatsheet-cheat-groups))))

(defun prolusion/cheatsheet-add-group (group &rest cheats)
  "Add cheats to the same group."
  (mapcar #'(lambda (cheat) (apply 'prolusion/cheatsheet-add (append `(:group ,group) cheat))) cheats))

;;;###autoload

(defun prolusion/cheatsheet-show ()
  "Create buffer and show cheatsheet."
  (interactive)
  (switch-to-buffer-other-window "*cheatsheet*")
  (prolusion/cheatsheet-mode)
  (erase-buffer)
  (insert (prolusion/cheatsheet-format))
  (goto-char (point-min))
  (setq buffer-read-only t))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cheatsheet setup
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(prolusion/cheatsheet-add-group 'Prolusion-Packages
    '(:key "C-u u" :description "prolusion/upgrade")
    '(:key "C-u p" :description "prolusion/upgrade-packages"))

(prolusion/cheatsheet-add-group 'Prolusion-Behavior
    '(:key "C-x o"               :description "other-window")
    '(:key "C-x O"               :description "other-window -1")
    '(:key "C-+"                 :description "text-scale-increase")
    '(:key "C--"                 :description "text-scale-decrease")
    '(:key "C-x ("               :description "start-kbd-macro")
    '(:key "C-x )"               :description "  end-kbd-macro")
    '(:key "C-x e"               :description "call-last-kbd-macro")
    '(:key "C-M-f"               :description "forward-sexp")
    '(:key "C-M-b"               :description "backward-sexp")
    '(:key "C-x n n"             :description "narrow-to-region")
    '(:key "C-x n w"             :description "widen")
    '(:key "M-x resize-window n" :description "resize-window--enlarge-down")
    '(:key "M-x resize-window p" :description "resize-window--enlarge-up")
    '(:key "M-x resize-window f" :description "resize-window--enlarge-horizontally")
    '(:key "M-x resize-window b" :description "resize-window--shrink-horizontally")
    '(:key "M-x resize-window r" :description "resize-window--reset-windows")
    '(:key "M-x resize-window w" :description "resize-window--cycle-windows-positive")
    '(:key "M-x resize-window W" :description "resize-window--cycle-windows-negative"))

(prolusion/cheatsheet-add-group 'Prolusion-Builtin
    '(:key "C-c b c" :description "clear-rectangle")
    '(:key "C-c b d" :description "delete-rectangle")
    '(:key "C-c b k" :description "kill-rectangle")
    '(:key "C-c b o" :description "open-rectangle")
    '(:key "C-c b t" :description "string-rectangle")
    '(:key "C-c b y" :description "yank-rectangle")
    '(:key "C-c b w" :description "wdired-change-to-wdired-mode")
    '(:key "C-c b s" :description "bookmark-set")
    '(:key "C-c b j" :description "bookmark-jump")
    '(:key "C-c b l" :description "bookmark-bmenu-list"))

(prolusion/cheatsheet-add-group 'Prolusion-Builtin
    '(:key "C-c l l" :description "mutli-eshell")
    '(:key "C-c l o" :description "mutli-eshell-switch")
    '(:key "C-c l 0" :description "mutli-eshell-go-back")
    '(:key "C-c l c" :description "prolusion/eshell-clear-buffer"))

(prolusion/cheatsheet-add-group 'Prolusion-Editor
    '(:key "C-c e s" :description "ff-find-other-file")
    '(:key "C-c e m" :description "make-header")
    '(:key "C-c e c" :description "make-box-comment")
    '(:key "C-c e d" :description "make-divider")
    '(:key "C-c e r" :description "make-revision")
    '(:key "C-c e g" :description "update-file-header")
    '(:key "C-c e l" :description "prolusion/duplicate-line")
    '(:key "C-c e e" :description "iedit-mode")
    '(:key "C-c e f" :description "helm-mini")
    '(:key "C-c e b" :description "helm-buffers-list")
    '(:key "C-c e k" :description "helm-show-kill-ring"))

(prolusion/cheatsheet-add-group 'Prolusion-Modes
    '(:key "C-c m p a" :description "conda-env-activate")
    '(:key "C-c m p d" :description "conda-env-deactivate")
    '(:key "C-c m p l" :description "conda-env-list")
    '(:key "C-c m r a" :description "global-rbenv-mode"))

(prolusion/cheatsheet-add-group 'Prolusion-Snippets
    '(:key "C-c y n" :description "yas-new-snippet")
    '(:key "C-c y s" :description "yas-insert-snippet")
    '(:key "C-c y v" :description "yas-visit-snippet-file"))

(prolusion/cheatsheet-add-group 'Prolusion-VC
    '(:key "C-c v m" :description "magit-status"))

(prolusion/cheatsheet-add-group 'Prolusion-Projectile
    '(:key "C-c p h"   :description "helm-projectile")
    '(:key "C-c p a"   :description "helm-projectile-find-other-file")
    '(:key "C-c p f"   :description "helm-projectile-find-file")
    '(:key "C-c p F"   :description "helm-projectile-find-file-in-known-projects")
    '(:key "C-c p g"   :description "helm-projectile-find-file-dwim")
    '(:key "C-c p d"   :description "helm-projectile-find-dir")
    '(:key "C-c p p"   :description "helm-projectile-switch-project")
    '(:key "C-c p e"   :description "helm-projectile-recentf")
    '(:key "C-c p b"   :description "helm-projectile-switch-to-buffer")
    '(:key "C-c p s g" :description "helm-projectile-grep")
    '(:key "C-c p s a" :description "helm-projectile-ack")
    '(:key "C-c p s s" :description "helm-projectile-ag"))

(prolusion/cheatsheet-add-group 'Prolusion-Completion
    '(:key "C-c c c" :description "anaconda-mode-complete")
    '(:key "C-c c d" :description "anaconda-mode-find-definitions")
    '(:key "C-c c a" :description "anaconda-mode-find-assignments")
    '(:key "C-c c r" :description "anaconda-mode-find-references")
    '(:key "C-c c b" :description "anaconda-mode-go-back")
    '(:key "C-c c s" :description "anaconda-mode-show-doc"))

(prolusion/cheatsheet-add-group 'Prolusion-Tagging
    '(:key "C-c t h" :description "helm-gtags-display-browser")
    '(:key "C-c t P" :description "helm-gtags-find-files")
    '(:key "C-c t f" :description "helm-gtags-parse-file")
    '(:key "C-c t g" :description "helm-gtags-find-pattern")
    '(:key "C-c t s" :description "helm-gtags-find-symbol")
    '(:key "C-c t r" :description "helm-gtags-find-rtag")
    '(:key "C-c t t" :description "helm-gtags-find-tag")
    '(:key "C-c t d" :description "helm-gtags-find-tag"))

(prolusion/cheatsheet-add-group 'Prolusion-Checking
    '(:key "C-c ! l" :description "flycheck-list-errors")
    '(:key "C-c ! n" :description "flycheck-next-error")
    '(:key "C-c ! p" :description "flycheck-previous-error")
    '(:key "C-c ! s" :description "flycheck-select-checker")
    '(:key "C-c ! x" :description "flycheck-disable-checker"))

(prolusion/cheatsheet-add-group 'Prolusion-Workspaces
    '(:key "C-c w n" :description "persp-next")
    '(:key "C-c w p" :description "persp-prev")
    '(:key "C-c w s" :description "persp-frame-switch")
    '(:key "C-c w S" :description "persp-window-switch")
    '(:key "C-c w r" :description "persp-rename")
    '(:key "C-c w c" :description "persp-kill")
    '(:key "C-c w a" :description "persp-add-buffer")
    '(:key "C-c w t" :description "persp-temporarily-display-buffer")
    '(:key "C-c w i" :description "persp-import-buffers")
    '(:key "C-c w k" :description "persp-remove-buffer")
    '(:key "C-c w K" :description "persp-kill-buffer")
    '(:key "C-c w w" :description "persp-save-state-to-file")
    '(:key "C-c w l" :description "persp-load-state-from-file"))

(prolusion/cheatsheet-add-group 'Prolusion-Typing
    '(:key "C-c y b" :description "speed-type-buffer")
    '(:key "C-c y r" :description "speed-type-region")
    '(:key "C-c y t" :description "speed-type-text"))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
;;
;; (cheatsheet-add-group 'GroupName
;;            '(:key "key1" :description "desc1")
;;            '(:key "key2" :description "desc2"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; prolusion-cheatsheet.el ends here
