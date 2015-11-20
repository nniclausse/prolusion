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

(prolusion-require-package 'exec-path-from-shell)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "CMAKE_PREFIX_PATH"))

(setq eshell-directory-name prolusion-eshell-dir)

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(defun prolusion-eshell-clear-buffer () "Clear eshell buffer"
       (interactive)
       (let ((inhibit-read-only t))
         (erase-buffer)
         (eshell-send-input)))

(add-hook 'eshell-mode-hook
          '(lambda() (local-set-key (kbd "C-l") 'prolusion-eshell-clear-buffer)))

(provide 'prolusion-eshell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; prolusion-eshell.el ends here
