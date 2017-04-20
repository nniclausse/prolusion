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

(defgroup prolusion-tree nil
  "Options for doom's neotree theme"
  :group 'doom-themes)

(defface prolusion-tree-dir-face  '((t (:inherit header-line)))
  "Face for directory labels."
  :group 'prolusion-tree)

(defface prolusion-tree-file-face '((t (:inherit act1)))
  "Face for file name labels."
  :group 'prolusion-tree)

(defface prolusion-tree-hidden-file-face '((t (:inherit font-lock-comment-face)))
  "Face for labels of hidden files. See `prolusion-tree-file-face-re-alist'."
  :group 'prolusion-tree)

(defface prolusion-tree-text-file-face '((t (:inherit func)))
  "Face for labels of text/documentation files (readmes, org files, etc). See
`prolusion-tree-file-face-re-alist'."
  :group 'prolusion-tree)

(defface prolusion-tree-media-file-face '((t (:inherit func)))
  "Face for labels of media files. See `prolusion-tree-file-face-re-alist'."
  :group 'prolusion-tree)

(defface prolusion-tree-data-file-face '((t (:inherit func)))
  "Face for labels of data files (json, yaml, xml, etc). See
`prolusion-tree-file-face-re-alist'."
  :group 'prolusion-tree)

(defcustom prolusion-tree-project-size 1.4
  "What :height to display the project icon at the top at."
  :type 'float
  :group 'prolusion-tree)

(defcustom prolusion-tree-folder-size 1.05
  "What :height to display the folder icons at."
  :type 'float
  :group 'prolusion-tree)

(defcustom prolusion-tree-chevron-size 0.8
  "What :height to display the chevron icons at."
  :type 'float
  :group 'prolusion-tree)

(defcustom prolusion-tree-line-spacing 2
  "Line-spacing for neotree buffer."
  :type 'symbol
  :group 'prolusion-tree)

(define-obsolete-variable-alias 'prolusion-tree-enable-file-icons 'prolusion-tree-file-icons)
(defcustom prolusion-tree-file-icons 'simple
  "The style to use for the file icons. Can be nil (disabled), non-nil (for a
diverse iconset), or 'simple, which is closest's to Atom's style as it only
distinguishes text, source, pdfs, images and binary files."
  :type '(choice
          (const :tag "A diverse array of file icons based on file type" t)
          (const :tag "Minimalistic file icons (like Atom's)" 'simple)
          (const :tag "Disable file icons" nil))
  :group 'prolusion-tree)

(defcustom prolusion-tree-enable-folder-icons t
  "If non-nil, display folder icons next to each file. Different icons are used
depending on whether the folder is a repo, symlink or regular folder."
  :type 'boolean
  :group 'prolusion-tree)

(defcustom prolusion-tree-enable-open-chevron-icons t
  "If non-nil, display the chevron-down icon next to each expanded folder."
  :type 'boolean
  :group 'prolusion-tree)

(defcustom prolusion-tree-enable-closed-chevron-icons t
  "If non-nil, display the chevron-right icon next to each collapsed folder."
  :type 'boolean
  :group 'prolusion-tree)

(defcustom prolusion-tree-enable-variable-pitch nil
  "If non-nil, labels will use the `prolusion-tree-dir-face' and
`prolusion-tree-dir-face' faces, which inherit from the `variable-pitch' face."
  :type 'boolean
  :group 'prolusion-tree)

(defcustom prolusion-tree-enable-type-colors t
  "If non-nil, color each file/folder based on the categories determined by
`prolusion-tree-file-face-re-alist'."
  :type 'boolean
  :group 'prolusion-tree)

(defcustom prolusion-tree-file-face-re-alist
  '(("\\(/\\.[^$/]+\\|\\.\\(lock\\|resolved\\|o\\|pyc\\|elc\\)$\\|/\\(node_modules\\|vendor\\)[/$]\\)"
     . prolusion-tree-hidden-file-face)
    ("\\(\\.\\(md\\|org\\|rst\\|log\\)\\|/[A-Z_-]+\\(\\.[a-z]+\\)?\\)$"
     . prolusion-tree-text-file-face)
    ("\\.\\(png\\|jpe?g\\|gif\\|tiff\\|svg\\|bmp\\|mov\\|avi\\|mp[34]\\|webm\\|zip\\|tar\\(\\.gz\\)?\\|7z\\|rar\\)$"
     . prolusion-tree-media-file-face)
    ("\\.\\([jc]son\\|\\(ya?\\|x\\|to\\)ml\\|xml\\)"
     . prolusion-tree-data-file-face))
  "Regexps used to determine what category each file/folder belongs to, and what
face to assign them."
  :type '(repeat (cons (regexp :tag "Pattern")
                       (symbol :tag "Face")))
  :group 'prolusion-tree)

(defvar prolusion//tree-file-re
  `((code    . ,(concat "\\.\\(p?html?\\|xml\\|ya?ml\\|json\\|tpl\\|conf\\|erb\\|mustache\\|twig\\|ejs\\|haml\\|pug\\|jade\\)$"))
    (media   . ,(concat "\\.\\("
                        "png\\|jpe?g\\|gif\\|tiff\\|svg\\|bmp"
                        "\\|mov\\|avi\\|mp[34]\\|webm"
                        "\\)$"
                        ))
    (archive . "\\.\\(zip\\|rar\\|7z\\|tar\\(\\.gz\\)?\\)$"))
  "An alist mapping file type to regular expressions, used to determine what
type of icon to display for the file if `prolusion-tree-file-icons' is set to
`simple'.")

(defun prolusion//tree-no-fringes ()
  "Remove fringes in neotree. They get reset each time you select the neotree
pane and are highlighted incorrectly."
  (set-window-fringes neo-global--window 1 0))

(defun prolusion//tree-setup (&rest _)
  (setq line-spacing prolusion-tree-line-spacing
        tab-width 1)
  (when (featurep 'hl-line)
    (set (make-local-variable 'hl-line-sticky-flag) t)
    (hl-line-mode +1)))

(defun prolusion//tree-folder-icon-for (dir chevron &optional faces)
  (let* ((path (expand-file-name dir))
         (chevron
          (if chevron
              (all-the-icons-octicon
               (format "chevron-%s" chevron)
               :v-adjust 0.1
               :face `(:inherit (,@faces)
                       :family ,(all-the-icons-octicon-family)
                       :height ,prolusion-tree-chevron-size))
            spc))
         (icon
          (when prolusion-tree-enable-folder-icons
            (all-the-icons-octicon
             (cond ((file-symlink-p path) "file-symlink-directory")
                   ((file-exists-p (format "%s/.git" path)) "file-submodule")
                   ((all-the-icons-dir-is-submodule path) "file-submodule")
                   (t "file-directory"))
             :v-adjust 0
             :face `(:inherit (,@faces)
                     :family ,(all-the-icons-octicon-family)
                     :height ,prolusion-tree-folder-size)))))
    (concat chevron "\t" icon)))

(defun prolusion//tree-file-icon-for (file-name &optional faces)
  (cond ((eq prolusion-tree-file-icons 'simple)
         (if file-name
             (propertize
               (cond ((string-match-p (cdr (assq 'code prolusion//tree-file-re)) file-name)
                      (all-the-icons-octicon "file-code"))
                     ((string-match-p (cdr (assq 'media prolusion//tree-file-re)) file-name)
                      (all-the-icons-octicon "file-media"))
                     ((string-match-p (cdr (assq 'archive prolusion//tree-file-re)) file-name)
                      (all-the-icons-octicon "file-zip"))
                     ((string= (or (file-name-extension file-name) "") "pdf")
                      (all-the-icons-octicon "file-pdf"))
                     ((file-symlink-p file-name)
                      (all-the-icons-octicon "file-symlink-file"))
                     ((file-executable-p file-name)
                      (all-the-icons-octicon "file-binary"))
                     (t
                      (all-the-icons-octicon "file-text")))
               'face `(:inherit (,@faces)
                       :family ,(all-the-icons-octicon-family)
                       :height 1.3)
               'display '(raise 0))
           (all-the-icons-fileicon "default")))
        (t (all-the-icons-icon-for-file file-name))))

(defun prolusion//tree-insert-fold-symbol (type file-name &optional faces)
  "Custom hybrid unicode theme with leading whitespace."
  (let ((spc "\t")
        (vspc (propertize "  " 'face 'variable-pitch)))
    (or (and (eq type 'open)
             (insert
              (concat spc
                      (prolusion//tree-folder-icon-for
                       file-name
                       (if prolusion-tree-enable-open-chevron-icons "down")
                       faces)
                      vspc)))
        (and (eq type 'close)
             (insert
              (concat spc
                      (prolusion//tree-folder-icon-for
                       file-name
                       (if prolusion-tree-enable-closed-chevron-icons "right")
                       faces)
                      vspc)))
        (and (eq type 'leaf)
             (insert
              (concat (when (or prolusion-tree-enable-open-chevron-icons
                                prolusion-tree-enable-closed-chevron-icons)
                        spc)
                      (when prolusion-tree-enable-folder-icons spc)
                      (when prolusion-tree-file-icons
                        (concat spc (prolusion//tree-file-icon-for file-name faces)))
                      vspc))))))

(defun prolusion//tree-get-file-face (name)
  (when prolusion-tree-enable-type-colors
    (let ((name (concat "/" (file-relative-name name neo-buffer--start-node)))
          case-fold-search)
      (cdr-safe
       (cl-find-if (lambda (re) (string-match-p (car re) name))
                   prolusion-tree-file-face-re-alist)))))

(defun prolusion//tree-buffer--insert-root-entry (node)
  "Pretty-print pwd in neotree"
  (let ((project-name (file-name-nondirectory (substring node 0 (1- (length node)))))
        (faces '(neo-root-dir-face)))
    (when prolusion-tree-enable-variable-pitch
      (push 'variable-pitch faces))
    (if (display-graphic-p)
        (insert
         (concat (propertize " " 'face `(:inherit (,@faces)))
                 (all-the-icons-octicon "repo"
                                        :height prolusion-tree-project-size
                                        :face 'neo-root-dir-face
                                        :v-adjust -0.1)
                 (propertize " " 'face 'neo-root-dir-face))))
    (insert (propertize (concat project-name "\n") 'face `(:inherit (,@faces))))))

(eval-after-load "neotree"
  (lambda ()
    (require 'all-the-icons)
    (advice-add 'neo-buffer--insert-root-entry :override 'prolusion//tree-buffer--insert-root-entry)))

(provide 'prolusion-tree)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; prolusion-tree.el ends here
