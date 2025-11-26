;;; keys.el ---

;; Copyright (C) 2005 Nicolas Niclausse
;;
;; Author: Nicolas.Niclausse@sophia.inria.fr
;; Version: $Id$
;; Keywords:
;; Requirements:
;; Status: not intended to be distributed yet

;(setq-default tab-width 4)
(setq-default visible-bell t)

(require 'server)
(server-start)

(iswitchb-mode 1)

(setq compilation-scroll-output 'first-error)


;; disable this !@#%& of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

(global-unset-key [(control x) (control q)]) ; Trop proche de Ctrl-x Ctrl-s
(global-unset-key [(control x) s]) ; Idem

;; On indique que les lignes doivent etre coupées AVEC le CR
(set 'kill-whole-line t)

(require 'redo+)
(global-set-key [(meta f12)] 'redo)

;; Silently add a newline at the end of a file if none is there
(setq require-final-newline t)

;; Overwrite mode must be the worlds most infuriating tool ever
(global-set-key '[insert] 'insert-selection)


;; Comportement type 'PC' de la région (la zone de texte sélectionnée
;; est visible, et est détruite lorsqu'on tape du texte). On ne
;; l'active pas pour les vieux emacs en mode tty, car il ne peuvent pas
;; marquer visuellement la région (cette feature devient alors
;; dangereuse).
(cond ((featurep 'xemacs)
       (progn (require 'pending-del)
              (custom-set-variables '(pending-delete-mode t t))
              ))
      ((or window-system (string-match "21" emacs-version))
       (progn (delete-selection-mode 1) (transient-mark-mode 1)
              ))
      )



(load-file (expand-file-name "~/.emacs.d/shift_mark.el"))

(setq erlang-root-dir "/usr/lib64/erlang")
(add-hook 'erlang-mode-hook
          '(lambda()
             (local-set-key  "\C-cm" 'erlang-man-function)))

;; Revient � l'endroit de derni�re modification du buffer. Tr�s pratique !
(defun goto-last-change ()
  (interactive)
  "Moves point back to the last change, based on buffer-undo-list"
  (typecase (first (first (rest buffer-undo-list)))
    (string
     (goto-char (abs (rest (first (rest buffer-undo-list))))))
    (integer
     (goto-char (rest (first (rest buffer-undo-list)))))
    (t
     (message "Unable to figure out where the last change was"))))

(global-set-key '[(control  f12)] 'goto-last-change)

(global-set-key [(meta g)] 'goto-line)

(global-set-key '[(control right)] 'end-of-line)
(global-set-key '[(alt right)] 'forward-sexp)
(global-set-key '[(alt left)] 'backward-sexp)
(global-set-key '[(meta left)] 'backward-word)
(global-set-key '[(meta right)] 'forward-word)
(global-set-key '[(control down)] 'scroll-up)
(global-set-key '[(control up)] 'scroll-down)
(global-set-key '[(meta up)] 'backward-paragraph)
(global-set-key '[(meta down)] 'forward-paragraph)

;; useful if you want to  insert tabs into comments and such.
(define-key global-map '[(shift tab)] 'self-insert-command)

;; Pour compiler
(setq compile-command "make ")
(setq compilation-read-command t)
(defun do-make nil (interactive) "\
Cette commande lance le make. Plus précisément, elle execute la commande shell stockée
dans la variable compile-command" (compile compile-command))
(global-set-key '[f9] 'do-make) ; non interactive: utilise la valeur de compile-command
(global-set-key '[(shift f9)] 'compile) ; demande confirmation

;; Très pratique pour débugger
(global-set-key '[f10] 'next-error)
(global-set-key '[(shift f10)] 'previous-error)

;; Virer tout les espaces sauf un autour du curseur : meta-space !

;; Chercher-remplacer
(global-set-key '[(meta f1)] 'query-replace)

; Pour commenter une région
(global-set-key '[f11] 'comment-region)
;; Pour décommenter une région
(global-set-key '[(shift f11)] '[(control u) f11])

(global-set-key '[f4] 'new-frame)
(global-set-key '[(shift f4)] 'magit-status)

;; desktop
;; (global-set-key '[f6] 'desktop-read)
;; (global-set-key '[(shift f6)] 'desktop-save)

;; execute macro
(global-set-key '[f6] 'kmacro-start-macro-or-insert-counter)
(global-set-key '[f7] 'kmacro-end-or-call-macro)

;; ------------------ Les commandes pour les buffers et les fen�tres

;; Un raccourci pour afficher la liste
(global-set-key '[f5] 'list-buffers)
;; Un raccourci pour y aller
(global-set-key '[(shift f5)] 'Buffer-menu-1-window)

;; Evalue le buffer
(global-set-key '[f8] 'eval-buffer)

;; D�place le curseur d'une fen�tre (buffer) � l'autre
(global-set-key '[(meta next)] 'next-multiframe-window)
(global-set-key '[(meta prior)] 'previous-multiframe-window)

;; Switche d'un buffer à l'autre
(defun qc-rotate-buffer (&optional argp)
  "Switch to the next buffer in the list in current window.
With C-u or a argument switch to the previous buffer."
  (interactive "P")
  (if (not argp) (bury-buffer (current-buffer)))
  (let ((buffers (if argp (reverse (buffer-list)) (buffer-list)))
   buffer found)
    (while (and buffers (not found))
      (setq buffer (buffer-name (car buffers)))
      (if (null (string-match "\\*.*\\*" buffer)) ;; on vire rien en fait
          (setq found t)
        (setq buffers (cdr buffers)))
      )
    (if found (switch-to-buffer buffer))))

(global-set-key '[(control next)]   '(lambda () (interactive) (qc-rotate-buffer 1)))
(global-set-key '[(control prior)] 'qc-rotate-buffer)

(global-set-key '[(meta a)] 'backward-word)
(global-set-key '[(meta e)] 'forward-word)

;; Pour augmenter/diminuer de 1 ligne un buffer
(global-set-key '[(control kp_subtract)] 'shrink-window)
(global-set-key '[(control kp_add)] 'enlarge-window)

(global-set-key (kbd "C-x C-q") 'dired-toggle-read-only)
(global-set-key (kbd "C-c q") 'zeal-at-point)

(global-set-key '[(control *)] 'split-window-vertically)
(global-set-key '[(control kp_multiply)] 'split-window-vertically)
(global-set-key '[(control kp_divide)] 'delete-window)

;; Vire la fenêtre où l'on se trouve en ce moment
(global-set-key '[(shift backspace)] 'delete-window)

;; Vire toutes les fenêtres sauf celle où on se trouve
(global-set-key '[(shift delete)] 'delete-other-windows)


;; ------------------------------- Les commandes de blocs

;; Définit le début du bloc
(global-set-key '[f1] 'set-mark-command)
;; Copie le bloc
(global-set-key '[f2] 'kill-ring-save)
;; Coupe le bloc (kill veut dire couper (ils zont pas trouvé plus con ?))
(global-set-key '[(shift f2)] 'kill-region)
;; Colle le bloc préalablement coupé (yank veut dire coller )
(global-set-key '[f3] 'yank)

(global-set-key '[(shift f3)] 'jde-debug-step-over)

;; Une touche undo en +
(global-set-key '[f12] 'undo)

(global-set-key '[(control delete)] 'kill-line)
(global-set-key '[(meta delete)] 'kill-word)
(global-set-key '[(meta control delete)] 'kill-sexp)
(global-set-key '[(control backspace)] 'backward-kill-line)
(global-set-key '[(meta backspace)] 'backward-kill-word)
(global-set-key '[(meta control backspace)] 'backward-kill-sexp)

(global-set-key '[(control insert)] 'yank)
(global-set-key '[(meta insert)] 'yank)


(defun cleanup-org-tables ()
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "-+-" nil t) (replace-match "-|-"))
    ))

;(add-hook 'c-mode-common-hook
;  (lambda()
;    (local-set-key  (kbd "C-c o") 'ff-find-other-file)))

(map! ;;

 "C-+"   'text-scale-increase
 "C--"   'text-scale-decrease

 :leader

 "e" nil
 (:prefix ("e" . "editor")

  :desc "Switch header/source" "o" #'ff-find-other-file
  )
)
                                        ;(:after c-mode
 ; (:map c-mode-map
 ;  "C-c o" #'ff-find-other-file))
 ;)

; 070917: no longer works ???
; (add-hook 'markdown-mode-hook 'orgtbl-mode)
;(add-hook 'markdown-mode-hook
;          (lambda()
;            (add-hook 'after-save-hook 'cleanup-org-tables  nil 'make-it-local)))

;;; keys.el ends here
