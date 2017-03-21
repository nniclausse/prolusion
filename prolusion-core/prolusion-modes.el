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
;; Modes requirements
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(prolusion/require-package 'rbenv)
(prolusion/require-package 'conda)
(prolusion/install-package 'cmake-mode)
(prolusion/install-package 'csharp-mode)
(prolusion/install-package 'markdown-mode)
(prolusion/install-package 'yaml-mode)
(prolusion/install-package 'js2-mode)
(prolusion/install-package 'js3-mode)
(prolusion/install-package 'json-mode)
(prolusion/install-package 'json-reformat)
(prolusion/install-package 'json-snatcher)
(prolusion/install-package 'tern)
(prolusion/install-package 'qml-mode)
(prolusion/install-package 'anaconda-mode)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modes setup
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("\\.h\\'"           . c++-mode))
(add-to-list 'auto-mode-alist '("\\.c\\'"           . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cc$"            . c++-mode))
(add-to-list 'auto-mode-alist '("\\.C$"             . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cpp$"           . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cxx$"           . c++-mode))
(add-to-list 'auto-mode-alist '("\\.tpp\\'"         . c++-mode))
(add-to-list 'auto-mode-alist '("\\.tcc$"           . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hpp$"           . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hxx$"           . c++-mode))
(add-to-list 'auto-mode-alist '("\\.qdoc$"          . c++-mode))
(add-to-list 'auto-mode-alist '(".gitignore\\'"     . makefile-mode))
(add-to-list 'auto-mode-alist '(".gitattributes\\'" . makefile-mode))
(add-to-list 'auto-mode-alist '("qmldir\\'"         . makefile-mode))
(add-to-list 'auto-mode-alist '("\\.pr[io]\\'"      . makefile-mode))
(add-to-list 'auto-mode-alist '("\\.qrc\\'"         . xml-mode))
(add-to-list 'auto-mode-alist '("\\.info\\'"        . info-mode))
(add-to-list 'auto-mode-alist '("\\.qmltypes\\'"    . json-mode))
(add-to-list 'auto-mode-alist '("\\.ejs\\'"         . html-mode))
(add-to-list 'auto-mode-alist '(".mbsyncrc\\'"      . conf-mode))
(add-to-list 'auto-mode-alist '(".msmtprc\\'"       . conf-mode))
(add-to-list 'auto-mode-alist '("mbsyncrc\\'"       . conf-mode))
(add-to-list 'auto-mode-alist '("msmtprc\\'"        . conf-mode))
(add-to-list 'auto-mode-alist '("doxyfile\\'"       . conf-mode))
(add-to-list 'auto-mode-alist '("doxyfile.in\\'"    . conf-mode))
(add-to-list 'auto-mode-alist '("Doxyfile\\'"       . conf-mode))

(use-package      cmake-mode :mode "\\.cmake\\'" "\\CMakeLists.txt\\'")
(use-package javascript-mode :mode "\\.qs\\'")
(use-package       yaml-mode :mode "\\.yml\\'")
(use-package        js2-mode :mode "\\.js\\'")
(use-package       json-mode :mode "\\.json\\'")
(use-package        qml-mode :mode "\\.qml\\'")

(setenv "PATH" (concat (getenv "HOME") "/.rbenv/shims:" (getenv "HOME") "/.rbenv/bin:" (getenv "PATH")))

(setq exec-path (cons (concat (getenv "HOME") "/.rbenv/shims") (cons (concat (getenv "HOME") "/.rbenv/bin") exec-path)))

(custom-set-variables '(conda-anaconda-home "~/.conda"))

(conda-env-initialize-interactive-shells)
(conda-env-initialize-eshell)
(conda-env-autoactivate-mode nil)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Toogle between source and header
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq cc-other-file-alist
      '(("\\.c\\'"   (".h"))
        ("\\.C\\'"   (".h" ".hpp" ".hxx"))
        ("\\.cc\\'"  (".h" ".hpp" ".hxx"))
        ("\\.cpp\\'" (".h" ".hpp" ".hxx"))
        ("\\.cxx\\'" (".h" ".hpp" ".hxx"))
        ("\\.tpp\\'" (".h" ".hpp" ".hxx"))
        ("\\.tcc\\'" (".h" ".hpp" ".hxx"))
        ("\\.h\\'"   (".tpp" ".cpp" ".cxx" ".tcc" ".cc" ".C" ".c" ".hxx" ".hpp"))
        ("\\.hpp\\'" (".tpp" ".cpp" ".cxx" ".tcc" ".cc" ".C" ".c" ".h"))
        ("\\.hxx\\'" (".tpp" ".cpp" ".cxx" ".tcc" ".cc" ".C" ".c" ".h"))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modes functions
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun info-mode () ""
  (interactive)
  (let ((file-name (buffer-file-name)))
    (kill-buffer (current-buffer))
    (info file-name)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modes hooks
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook         'js-mode-hook             'tern-mode)
(add-hook        'css-mode-hook          'rainbow-mode)
(add-hook        'qml-mode-hook          'rainbow-mode)
(add-hook       'prog-mode-hook 'prettify-symbols-mode)
(add-hook 'emacs-lisp-mode-hook          'rainbow-mode)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modes modeline
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(diminish 'tern-mode)
(diminish 'prettify-symbols-mode)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editor keybindings
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-c m p a") 'conda-env-activate)
(global-set-key (kbd "C-c m p d") 'conda-env-deactivate)
(global-set-key (kbd "C-c m p l") 'conda-env-list)
(global-set-key (kbd "C-c m r a") 'global-rbenv-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'prolusion-modes)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; prolusion-modes.el ends here
