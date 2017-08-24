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

;; disable this !@#%& of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; Silently add a newline at the end of a file if none is there
(setq require-final-newline t)

;; Overwrite mode must be the worlds most infuriating tool ever
(global-set-key '[insert] 'insert-selection)

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

(global-set-key '[(meta a)] 'backward-word)
(global-set-key '[(meta e)] 'forward-word)

;; Pour augmenter/diminuer de 1 ligne un buffer
(global-set-key '[(control kp_subtract)] 'shrink-window)
;(global-set-key '[(control -)] 'shrink-window)
(global-set-key '[(control kp_add)] 'enlarge-window)
;(global-set-key '[(control +)] 'enlarge-window)

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

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


;;; keys.el ends here
