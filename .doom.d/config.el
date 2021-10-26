;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Ian Waudby-Smith"
      user-mail-address "iwaudbysmith@gmail.com")

;; Some useful functions for centering emacs frame,
;; courtesy of Christian Tietze (https://christiantietze.de/posts/2021/06/emacs-center-window-on-current-monitor/)
(defun my/frame-monitor-usable-height (&optional frame)
  "Return the usable height in pixels of the monitor of FRAME.
FRAME can be a frame name, a terminal name, or a frame.
If FRAME is omitted or nil, use currently selected frame.

Uses the monitor's workarea. See `display-monitor-attributes-list'."
  (cadddr (frame-monitor-workarea frame)))

(defun my/frame-monitor-usable-width (&optional frame)
  "Return the usable width in pixels of the monitor of FRAME.
FRAME can be a frame name, a terminal name, or a frame.
If FRAME is omitted or nil, use currently selected frame.

Uses the monitor's workarea. See `display-monitor-attributes-list'."
  (caddr (frame-monitor-workarea frame)))

(defun my/frame-monitor-usable-left (&optional frame)
  "Return the left-most usable pixel of the monitor of FRAME.
FRAME can be a frame name, a terminal name, or a frame.
If FRAME is omitted or nil, use currently selected frame.

Uses the monitor's workarea. See `display-monitor-attributes-list'."
  (car (frame-monitor-workarea frame)))

(defun my/frame-monitor-usable-top (&optional frame)
  "Return the top-most usable pixel of the monitor of FRAME.
FRAME can be a frame name, a terminal name, or a frame.
If FRAME is omitted or nil, use currently selected frame.

Uses the monitor's workarea. See `display-monitor-attributes-list'."
  (car (frame-monitor-workarea frame)))

(defun my/center-box (w h cw ch)
  "Center a box inside another box.

Returns a list of `(TOP LEFT)' representing the centered position
of the box `(w h)' inside the box `(cw ch)'."
  (list (/ (- cw w) 2) (/ (- ch h) 2)))

(defun my/frame-get-center (frame)
  "Return the center position of FRAME on it's display."
  (my/center-box (frame-pixel-width frame) (frame-pixel-height frame)
                 (my/frame-monitor-usable-width frame) (my/frame-monitor-usable-height frame)))

(defun my/frame-recenter (&optional frame)
  "Center a frame on the screen."
  (interactive)
  (let* ((frame (or (and (boundp 'frame) frame) (selected-frame)))
         (center (my/frame-get-center frame)))
    (apply 'set-frame-position (flatten-list (list frame center)))))



;; Define useful variables for setting frame position/size
(setq my/frame-padding 30)

(defun my/frame-width ()
  (- (my/frame-monitor-usable-width)
     (* my/frame-padding 2)))

(defun my/frame-height ()
  (- (my/frame-monitor-usable-height)
     (* my/frame-padding 2)))

(defun my/frame-top ()
  (+ (my/frame-monitor-usable-top)
     my/frame-padding))

(defun my/frame-left ()
  (+ (my/frame-monitor-usable-left)
     my/frame-padding))

(defun my/frame-resize ()
  (interactive)
  (set-frame-position (selected-frame)
                      (my/frame-left)
                      (my/frame-top))
  (set-frame-size (selected-frame)
                  (my/frame-width)
                  (my/frame-height)
                  t))
;; Resize frames pixel-wise
(setq frame-resize-pixelwise t)

(my/frame-resize)
(my/frame-recenter)
;; (add-hook 'after-init-hook #'my/frame-resize)

;; Use C-hjkl to move between windows
;; This overwrites some bindings I don't need.
;; e.g. to use C-h, can use "SPC h", and C-jkl are
;; unneeded due to evil mode.
(bind-key* "C-h"  'windmove-left)
(bind-key* "C-l"  'windmove-right)
(bind-key* "C-k"  'windmove-up)
(bind-key* "C-j"  'windmove-down)

;; Split windows using C-a l/j to keep consistent bindings with tmux
;; This does not overwrite anything as far as I can tell
(bind-key* "C-a l"  'evil-window-vsplit)
(bind-key* "C-a j"  'evil-window-split)

;; Switch to the new window after splitting
(setq evil-split-window-below t
      evil-vsplit-window-right t)

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
(global-set-key (kbd "C-c c") 'my/config)

(defalias 'my/config
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") #'my/goto-private-config-org-file)
    (define-key map (kbd "C") #'my/goto-private-config-file)
    (define-key map (kbd "i") #'my/goto-private-init-file)
    (define-key map (kbd "p") #'my/goto-private-packages-file)
    map) "Config-related bindings")

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
(setq doom-font (font-spec :family "Fira Mono" :size 15))
(setq doom-variable-pitch-font (font-spec :family "Fira Sans" :size 15))
(setq +zen-text-scale 0.25)

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
(defun my/open-org-directory ()
  (interactive) (ido-find-file-in-dir org-directory))
(global-set-key (kbd "C-c o")
                'my/open-org-directory)

;; Use custom todo keywords
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

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

(setq confirm-kill-emacs nil)

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
