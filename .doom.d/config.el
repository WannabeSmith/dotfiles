;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Ian Waudby-Smith"
      user-mail-address "iwaudbysmith@gmail.com")

;; Use C-hjkl to move between windows
(bind-key* "C-h"  'windmove-left)
(bind-key* "C-l"  'windmove-right)
(bind-key* "C-k"  'windmove-up)
(bind-key* "C-j"  'windmove-down)

;; Split windows using C-c l/j
;; (global-set-key (kbd "C-c l") 'evil-window-vsplit)
;; (global-set-key (kbd "C-c j") 'evil-window-split)

;; Make default latex viewer pdf-tools
(setq +latex-viewers '(pdf-tools))

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

;; Make autocomplete less clunky: https://github.com/hlissner/doom-emacs/issues/77
(require 'company)
(setq company-idle-delay 0.2
      company-minimum-prefix-length 4)

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
(setq doom-font (font-spec :family "Fira Mono" :size 14))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-nord)

;; This directory allows syncing with beorg on iOS. Pretty cool!
(setq
 org-directory
 "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/")

(custom-set-faces
 '(org-level-1 ((t (:inherit outline-1 :height 1.3))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.1))))
 '(org-level-3 ((t (:inherit outline-3 :height 1))))
 '(org-level-4 ((t (:inherit outline-4 :height 1))))
 '(org-level-5 ((t (:inherit outline-5 :height 1))))
 '(org-level-6 ((t (:inherit outline-6 :height 1))))
 '(org-level-7 ((t (:inherit outline-7 :height 1))))
 '(org-level-8 ((t (:inherit outline-8 :height 1))))
 )

;; Enable org-download
(require 'org-download)
(add-hook 'dired-mode-hook 'org-download-enable)

;; Allow "C-c o" to quickly open up a file in the org directory
(global-set-key (kbd "C-c o")
                (lambda ()
                  (interactive)
                  (ido-find-file-in-dir org-directory)))

;; Use custom todo keywords
(after! org
  (setq org-todo-keywords
        '((sequence "TODO(t)" "INPROGRESS(p)" "WAITING(w)" "IDEA(i)" "|" "DONE" "CANCELLED(c)"))))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

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
