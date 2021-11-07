;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Make emacs maximized one startup
;; Might be useful in the future: https://christiantietze.de/posts/2021/06/emacs-center-window-on-current-monitor/
(add-hook 'doom-init-ui-hook 'toggle-frame-maximized)

(add-hook 'after-init-hook 'global-visual-line-mode)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
;; (setq display-line-numbers-type 'nil)

(setq evil-escape-key-sequence nil)

(setq evil-split-window-below t
      evil-vsplit-window-right t)

(bind-key "C-c F" 'toggle-frame-maximized)

;; C-c c will contain all buffer-related bindings
(global-set-key (kbd "C-c b") 'my/buffer)

(defalias 'my/buffer
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "k") 'kill-buffer-and-window)
    map) "Buffer-related bindings")

;; "C-c f" for files/folders
;; (global-set-key (kbd "C-c f") 'my/files)

;; (defalias 'my/files
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map (kbd "t") 'my/treemacs)
;;     map) "Files-related bindings")

;; "C-c f" for "files" and "folders"
(global-set-key (kbd "C-c f") 'my/treemacs)

(defalias 'my/treemacs
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "t") 'treemacs) ;; Toggle treemacs
    (define-key map (kbd "a") 'treemacs-add-project-to-workspace)
    (define-key map (kbd "r") 'treemacs-remove-project-from-workspace)
    map) "Treemacs-related bindings")

(global-set-key (kbd "C-c m") 'my/<localleader>)

(defun my/bind-python-keys ()
  (defalias 'my/<localleader>
    (let ((map (make-sparse-keymap)))
      ;; REPL
      (define-key map (kbd "R") #'run-python)
      ;; Restart python
      (define-key map (kbd "r") #'pyvenv-restart-python)
      ;; Virtual environment
      (define-key map (kbd "v") #'pyvenv-activate)
      ;; Format
      (define-key map (kbd "f") #'+format/buffer)
      ;; LSP-related bindings
      (define-key map (kbd "l") #'my/lsp)
      map)))

(defun my/bind-python-lsp-keys ()
  (defalias 'my/lsp
    (let ((map (make-sparse-keymap)))
      ;; Restart lsp server
      (define-key map (kbd "r") #'lsp-workspace-restart)
      map)))

(add-hook 'python-mode-hook 'my/bind-python-keys)
(add-hook 'python-mode-hook
          '(lambda ()
             (add-hook 'lsp-mode-hook
                       #'my/bind-python-lsp-keys)))
;; (add-hook 'python-mode-hook 'python-black-on-save-mode)

(global-set-key (kbd "C-c s") 'my/spelling)

(defun my/bind-spell-fu-bindings ()
  (defalias 'my/spelling
    (let ((map (make-sparse-keymap)))
      ;; Add word to dictionary
      (define-key map (kbd "a") #'spell-fu-word-add)
      map)))

(add-hook 'spell-fu-mode-hook 'my/bind-spell-fu-bindings)

(defun my/latexmk ()
  (interactive)
  (TeX-command "LatexMk" #'TeX-master-file nil))

(defun my/bibtex ()
  (interactive)
  (TeX-command "BibTeX" #'TeX-master-file nil))

(defun my/latex-view ()
    (interactive)
  (TeX-command "View" #'TeX-master-file nil))

(defun my/bind-latex-keys ()
  (defalias 'my/<localleader>
    (let ((map (make-sparse-keymap)))
      ;; Compile
      (define-key map (kbd "c") #'my/latexmk)
      ;; Recompile BibTeX
      (define-key map (kbd "b") #'my/bibtex)
      ;; Word count
      (define-key map (kbd "w") #'tex-count-words)
      map)))

(add-hook 'LaTeX-mode-hook 'my/bind-latex-keys)

(defun my/python-lsp-ignore-venv ()
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]\\venv_*"))
(add-hook 'python-mode-hook
          '(lambda () (add-hook 'lsp-mode-hook 'my/python-lsp-ignore-venv)))

(defun my/venv_pattern ()
  "User-customizable virtual environment pattern"
  "venv_*")

(defun my/get-matching-project-root-files (regexp)
  "Find all root directories/files that begin with `regexp`"
  (seq-filter
   (lambda (x) (equal 0 (string-match-p regexp x)))
   (directory-files (projectile-project-root))))

(defun my/python-venv-auto-activate ()
  "Activate the virtual environment satisfying the pattern given by the function, my/venv_pattern if it's a unique match, otherwise do nothing"
  (interactive)
  (setq matching-venvs (my/get-matching-project-root-files (my/venv_pattern)))
  ;; If there's a unique match, set the venv. Otherwise, do nothing
  (when (equal (length matching-venvs) 1)
    (pyvenv-activate (concat (projectile-project-root) (car matching-venvs)))))

(add-hook 'python-mode-hook 'my/python-venv-auto-activate)

(defun my/latex-format-environment-on-save ()
  (add-hook 'after-save-hook #'LaTeX-fill-environment))

(defun my/latexmk-on-save ()
  "Run LatexMk after saving .tex files"
  (add-hook 'after-save-hook 'my/latexmk))

(add-hook 'LaTeX-mode-hook 'my/latexmk-on-save)

;; Make default latex viewer pdf-tools
;; (setq +latex-viewers '(pdf-tools))

;; Use pdf-tools to open PDF files
(setq TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-source-correlate-start-server t)

;; Update PDF buffers after successful LaTeX runs
(add-hook 'TeX-after-compilation-finished-functions
          #'TeX-revert-document-buffer)

;; Make AUCTeX ask for main tex file in multi-document structure
(setq-default TeX-master nil)

;; Prevent AUCTeX from inserting braces automatically
(setq TeX-electric-sub-and-superscript nil)

(setq
 org-directory
 "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/")

(defun my/open-org-directory ()
  (interactive) (ido-find-file-in-dir org-directory))

(global-set-key (kbd "C-c o") 'my/open-org-directory)

(custom-set-faces '(org-level-1 ((t (:inherit outline-1 :height 1.2)))))

(require 'org-download)
(add-hook 'dired-mode-hook 'org-download-enable)

(after! org
  (setq org-todo-keywords
        '((sequence "TODO(t)" "IN-PROGRESS(p)" "WAITING(w)"
                    "IDEA(i)" "|" "DONE" "CANCELLED(c)"))))

;; Set other todo colors according to the nord theme (https://www.nordtheme.com/)
(setq org-todo-keyword-faces
      '(("IN-PROGRESS" . "#88C0D0")
        ("WAITING" . "#5E81AC")
        ("IDEA" . "#EBCB8B")
        ("CANCELED" . "#BF616A"))
      )

(setq org-log-done 'time)

(global-set-key (kbd "C-c t") 'vterm)

;; Other shells are cool but I don't use them enough. Might uncomment later.
;; (defalias 'my/shells
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map (kbd "s") 'shell)
;;     (define-key map (kbd "e") 'eshell)
;;     (define-key map (kbd "t") 'term)
;;     (define-key map (kbd "v") 'vterm)
;;     map) "Shell-related bindings")

(defun my/goto-private-config-org-file ()
  "Open your private config.org file."
  (interactive)
  (find-file (expand-file-name "config.org" doom-private-dir)))

(defun my/goto-private-config-file ()
  "Open your private config.el file."
  (interactive)
  (find-file (expand-file-name "config.el" doom-private-dir)))

(defun my/goto-private-init-file ()
  "Open your private init.el file."
  (interactive)
  (find-file (expand-file-name "init.el" doom-private-dir)))

(defun my/goto-private-packages-file ()
  "Open your private packages.el file."
  (interactive)
  (find-file (expand-file-name "packages.el" doom-private-dir)))

;; C-c c will contain all config-related stuff
(global-set-key (kbd "C-c e") 'my/emacs-config)

(defalias 'my/emacs-config
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") #'my/goto-private-config-org-file)
    (define-key map (kbd "C") #'my/goto-private-config-file)
    (define-key map (kbd "i") #'my/goto-private-init-file)
    (define-key map (kbd "p") #'my/goto-private-packages-file)
    map) "Config-related bindings")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))
(setq doom-font (font-spec :family "Fira Mono" :size 15))
(setq doom-variable-pitch-font (font-spec :family "Fira Mono" :size 15))
(setq +zen-text-scale 0.25)

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-nord)

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Ian Waudby-Smith"
      user-mail-address "iwaudbysmith@gmail.com")

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Make autocomplete less clunky: https://github.com/hlissner/doom-emacs/issues/77
;; (require 'company)
;; (setq company-idle-delay 0.2
;;       company-minimum-prefix-length 4)
