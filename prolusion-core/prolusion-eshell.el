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
;; Eshell requirements
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(prolusion/require-package        'all-the-icons)
(prolusion/require-package         'multi-eshell)
(prolusion/require-package 'exec-path-from-shell)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Eshell setup
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq eshell-directory-name                                                       prolusion-save-dir)
(setq eshell-history-file-name       (expand-file-name "prolusion-eshell-history" prolusion-save-dir))
(setq eshell-last-dir-ring-file-name (expand-file-name "prolusion-eshell-lastdir" prolusion-save-dir))

(when (memq window-system '(mac ns))
  (setq exec-path-from-shell-arguments (quote ("-l")))
  (setq exec-path-from-shell-variables (quote ("PATH" "MANPATH" "CMAKE_PREFIX_PATH" "LC_ALL" "LANG" "LC_CTYPE" "TERM" "USER" "HOSTNAME" "HOME")))
  (exec-path-from-shell-initialize))

(setq multi-eshell-name "*eshell*")
(setq multi-eshell-shell-function (quote (eshell)))

(setq eshell-scroll-to-bottom-on-input 'all)
(setq eshell-error-if-no-glob t)
(setq eshell-hist-ignoredups t)
(setq eshell-save-history-on-exit t)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Eshell functions
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prolusion/eshell-clear-buffer ()
  ""
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(defun prolusion//current-directory-git-branch-string (pwd)
  ""
  (interactive)
  (when (and (eshell-search-path "git")
             (locate-dominating-file pwd ".git"))
    (let ((git-output (shell-command-to-string (concat "cd " pwd " && git branch | grep '\\*' | sed -e 's/^\\* //'"))))
      (if (> (length git-output) 0)
          (concat
           (all-the-icons-octicon "git-branch" :v-adjust 0.1)
           " ("
           (substring git-output 0 -1)
           ")")
        "(no branch)"))))

(defun prolusion//split-directory-prompt (directory)
  (if (string-match-p ".*/.*" directory)
      (list (file-name-directory directory) (file-name-base directory))
    (list "" directory)))

(setq eshell-prompt-function
      (lambda ()
        (let* ((directory (prolusion//split-directory-prompt (eshell/pwd)))
               (parent (car directory))
               (name (cadr directory))
               (branch (or (prolusion//current-directory-git-branch-string (eshell/pwd)) ""))
               (separator (or (if (prolusion//current-directory-git-branch-string (eshell/pwd)) " " ""))))

          (if prolusion-dark-variant
              (concat
               (propertize (getenv "USER") 'face `(:foreground "#ccccff"))
               (propertize "@" 'face `(:foreground "#ccccff"))
               (propertize (getenv "HOSTNAME") 'face `(:foreground "#ccccff"))
               " "
               (propertize parent 'face `(:foreground "#8888ff"))
               (propertize name   'face `(:foreground "#8888ff" :weight bold))
               separator
               (propertize branch 'face `(:foreground "green"))
               (propertize " $"   'face `(:weight ultra-bold))
               (propertize " "    'face `(:weight bold)))

            (concat
             (propertize (getenv "USER") 'face `(:foreground "#222222"))
             (propertize "@" 'face `(:foreground "#222222"))
             (propertize (getenv "HOSTNAME") 'face `(:foreground "#222222"))
             " "
             (propertize parent 'face `(:foreground "blue"))
             (propertize name   'face `(:foreground "blue" :weight bold))
             separator
             (propertize branch 'face `(:foreground "dark green"))
             (propertize " $"   'face `(:weight ultra-bold))
             (propertize " "    'face `(:weight bold)))))))

(setq eshell-highlight-prompt nil)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Eshell hooks
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'eshell-mode-hook (lambda () (setq-local global-hl-line-mode nil)))
(add-hook   'term-mode-hook (lambda () (setq-local global-hl-line-mode nil)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Eshell keybindings
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-c l l") 'multi-eshell)
(global-set-key (kbd "C-c l o") 'multi-eshell-switch)
(global-set-key (kbd "C-c l O") 'multi-eshell-go-back)
(global-set-key (kbd "C-c l c") 'prolusion/eshell-clear-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'prolusion-eshell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; prolusion-eshell.el ends here
