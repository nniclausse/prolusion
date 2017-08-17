;; Version: $Id$  -*- no-byte-compile: t -*-
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
;;
;; (package-initialize)

(setq gc-cons-threshold-default gc-cons-threshold)
(setq gc-cons-threshold (* 100 1024 1024))

(run-with-idle-timer 20 nil (lambda () (setq gc-cons-threshold gc-cons-threshold-default) (message nil)))

(defconst prolusion-dark t)
(defconst prolusion-version-major 1)
(defconst prolusion-version-minor 9)
(defconst prolusion-version-patch 0)

(defvar prolusion-dir          (file-name-directory load-file-name))
(defvar prolusion-core-dir     (expand-file-name "prolusion-core"     prolusion-dir))
(defvar prolusion-docs-dir     (expand-file-name "prolusion-docs"     prolusion-dir))
(defvar prolusion-elpa-dir     (expand-file-name "prolusion-elpa"     prolusion-dir))
(defvar prolusion-save-dir     (expand-file-name "prolusion-save"     prolusion-dir))
(defvar prolusion-info-dir     (expand-file-name "prolusion-info"     prolusion-dir))
(defvar prolusion-shell-dir    (expand-file-name "prolusion-shell"    prolusion-dir))
(defvar prolusion-irony-dir    (expand-file-name "prolusion-irony"    prolusion-dir))
(defvar prolusion-snippets-dir (expand-file-name "prolusion-snippets" prolusion-dir))
(defvar prolusion-jedi-dir     (expand-file-name "prolusion-jedi"     prolusion-dir))

(make-directory prolusion-docs-dir  t)
(make-directory prolusion-elpa-dir  t)
(make-directory prolusion-save-dir  t)
(make-directory prolusion-irony-dir t)

(add-to-list 'load-path prolusion-core-dir)

(require 'prolusion-packages)
(require 'prolusion-workspaces)
(require 'prolusion-ui)
(require 'prolusion-dashboard)
(require 'prolusion-modes)
(require 'prolusion-behavior)
(require 'prolusion-projectile)
(require 'prolusion-eshell)
(require 'prolusion-editor)
(require 'prolusion-snippets)
(require 'prolusion-irony)
(require 'prolusion-completion)
(require 'prolusion-checking)
(require 'prolusion-compilation)
(require 'prolusion-www)
(require 'prolusion-vc)
(require 'prolusion-builtins)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
