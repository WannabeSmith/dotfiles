;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set up straight.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Taken from https://github.com/raxod502/straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Users of Emacs versions >= 27 want this
;; (source: https://github.com/raxod502/straight.el#getting-started)
(setq package-enable-at-startup nil)

;; Install use-package
(straight-use-package 'use-package)

;; Make use-package use straight as the package manager
(setq straight-use-package-by-default t)

;; Set straight-recipe-repositories
(setq straight-recipe-repositories '(melpa gnu-elpa-mirror el-get emacsmirror-mirror))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load ~/.emacs.d/config.org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Get straight to load org prior to the config.org being run.
(straight-use-package 'org)

;; Load org file.
(require 'org)

;; Open the real version of a file when presented with a symlink.
(setq vc-follow-symlinks t)

;; Don't ask for confirmation.
(setq org-confirm-babel-evaluate nil)

;; Tangle config.org into config.el
(org-babel-tangle-file "~/.emacs.d/config.org" "~/.emacs.d/config.el")

;; Load config.el!
(load-file "~/.emacs.d/config.el")
