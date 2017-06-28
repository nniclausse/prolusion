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

(prolusion/require-package         'multi-eshell)
(prolusion/require-package 'exec-path-from-shell)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Eshell setup
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq eshell-directory-name                                                       prolusion-save-dir)
(setq eshell-history-file-name       (expand-file-name "prolusion-eshell-history" prolusion-save-dir))
(setq eshell-last-dir-ring-file-name (expand-file-name "prolusion-eshell-lastdir" prolusion-save-dir))
(setq eshell-rc-script               (expand-file-name "eshellrc"                prolusion-shell-dir))

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

(eval-after-load 'esh-opt
  '(progn
     (require 'em-term)
     (add-to-list 'eshell-visual-commands "ccmake")
     (add-to-list 'eshell-visual-subcommands '("git" '("log" "st" "status")))))

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
        (let* ((hostname (or (getenv "HOSTNAME") "localhost" (getenv "HOSTNAME")))
               (directory (prolusion//split-directory-prompt (replace-regexp-in-string (getenv "HOME") "~" (eshell/pwd))))
               (parent (car directory))
               (name (cadr directory))
               (branch (or (prolusion//current-directory-git-branch-string (eshell/pwd)) ""))
               (separator (if (prolusion//current-directory-git-branch-string (eshell/pwd)) " " ""))
               (icon (if (prolusion//current-directory-git-branch-string (eshell/pwd)) (propertize (all-the-icons-octicon "git-branch" :v-adjust 0.1) 'face `(:family ,(all-the-icons-octicon-family) :foreground "green")) "")))

          (concat
           (propertize (getenv "USER") 'face `(:foreground "#ccccff"))
           (propertize "@" 'face `(:foreground "#ccccff"))
           (propertize (car (split-string hostname "\\.")) 'face `(:foreground "#ccccff"))
           " "
           (propertize parent 'face `(:foreground "#8888ff"))
           (propertize name   'face `(:foreground "#8888ff" :weight bold))
           separator
           icon
           (propertize branch 'face `(:foreground "green"))
           (propertize " $"   'face `(:weight ultra-bold))
           (propertize " "    'face `(:weight bold))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Eshell hooks
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'eshell-mode-hook (lambda () (beacon-mode 0)))
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
