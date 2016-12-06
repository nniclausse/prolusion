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
;; Projectile requirements
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(prolusion-install-package              'ag)
(prolusion-install-package      'projectile)
(prolusion-install-package 'helm-projectile)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Projectile setup
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package projectile
  :config
  (projectile-global-mode)

  (setq projectile-enable-caching t)
  (setq projectile-completion-system 'helm)
  (setq projectile-known-projects-file (expand-file-name "prolusion-projectile-known" prolusion-save-dir))
  (setq projectile-cache-file          (expand-file-name "prolusion-projectile-cache" prolusion-save-dir)))

(use-package helm-projectile
  :after helm
  :bind (
  ("C-c p h" . helm-projectile)
  ("C-c p a" . helm-projectile-find-other-file)
  ("C-c p f" . helm-projectile-find-file)
  ("C-c p F" . helm-projectile-find-file-in-known-projects)
  ("C-c p g" . helm-projectile-find-file-dwim)
  ("C-c p d" . helm-projectile-find-dir)
  ("C-c p e" . helm-projectile-recentf)
  ("C-c p b" . helm-projectile-switch-to-buffer)
  ("C-c p p" . helm-projectile-switch-project)
  ("C-c p s g" . helm-projectile-grep)
  ("C-c p s a" . helm-projectile-ack)
  ("C-c p s s" . helm-projectile-ag)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Projectile modeline
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(diminish 'projectile-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'prolusion-projectile)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; prolusion-projectile.el ends here
