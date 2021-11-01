;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Make emacs maximized one startup
(add-hook 'doom-init-ui-hook 'toggle-frame-maximized)

;; Might be useful in the future: https://christiantietze.de/posts/2021/06/emacs-center-window-on-current-monitor/

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
(global-set-key (kbd "C-c f") 'my/treemacs)

(defalias 'my/treemacs
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "t") 'treemacs) ;; Toggle treemacs
    (define-key map (kbd "a") 'treemacs-add-project-to-workspace)
    (define-key map (kbd "r") 'treemacs-remove-project-from-workspace)
    map) "Treemacs-related bindings")

;; C-c c will contain all coding-related bindings
(global-set-key (kbd "C-c c") 'my/coding)

(defalias 'my/coding
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "p") 'my/python)
    (define-key map (kbd "r") 'my/r)
    (define-key map (kbd "c") 'my/cpp)
    (define-key map (kbd "j") 'my/julia)
    (define-key map (kbd "h") 'my/haskell)
    map) "Coding-related bindings")

(defalias 'my/python
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "r") #'run-python)
    (define-key map (kbd "v") #'pyvenv-activate)
    (define-key map (kbd "f") #'python-black-buffer)
    map) "Python-related bindings")

;; C-c w will contain all writing-related bindings
(global-set-key (kbd "C-c w") 'my/writing)

(defalias 'my/writing
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "t") 'my/tex)
    (define-key map (kbd "o") 'my/org-mode)
    map) "Writing-related bindings")

(defalias 'my/tex
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") 'tex-count-words)
    map))

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



;; Quickly open up a file in the org directory
(defun my/open-org-directory ()
  (interactive) (ido-find-file-in-dir org-directory))

(defalias 'my/org-mode
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "o") 'my/open-org-directory)
    map))

(setq
 org-directory
 "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/")

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

;; C-c s will contain all shell-related commands
(global-set-key (kbd "C-c s") 'my/shells)

(defalias 'my/shells
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "s") 'shell)
    (define-key map (kbd "e") 'eshell)
    (define-key map (kbd "t") 'term)
    (define-key map (kbd "v") 'vterm)
    map) "Shell-related bindings")

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
(global-set-key (kbd "C-c e") 'my/config)

(defalias 'my/config
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
