(defun my/set-initial-frame-size ()
  "Set the initial frame size to something reasonable. Works on multiple monitors"
  (interactive)
  (let* ((base-factor 0.93)
         (monitor-w (nth 2 (frame-monitor-workarea (selected-frame))))
         (monitor-h (nth 3 (frame-monitor-workarea (selected-frame))))
         (a-width (* monitor-w base-factor))
         (a-height (* monitor-h base-factor)))
    (set-frame-size (selected-frame)
                    (truncate a-width)
                    (truncate a-height) t)))

(defun my/frame-recenter (&optional frame)
  "Center FRAME on the screen.
FRAME can be a frame name, a terminal name, or a frame.
If FRAME is omitted or nil, use currently selected frame."
  (interactive)
  (unless (eq 'maximised (frame-parameter nil 'fullscreen))
    (let* ((frame (or (and (boundp 'frame)
                           frame)
                      (selected-frame)))
           (frame-w (frame-pixel-width frame))
           (frame-h (frame-pixel-height frame))
           ;; frame-monitor-workarea returns (x y width height) for the monitor
           (monitor-w (nth 2 (frame-monitor-workarea frame)))
           (monitor-h (nth 3 (frame-monitor-workarea frame)))
           (center (list (/ (- monitor-w frame-w) 2)
                         (/ (- monitor-h frame-h) 2))))
      (apply 'set-frame-position (flatten-list (list frame center))))))

(defun my/set-initial-frame ()
  (interactive)
  (my/set-initial-frame-size)
  (my/frame-recenter))

(setq frame-resize-pixelwise t)
(add-hook 'window-setup-hook #'my/set-initial-frame)

;; Convert the emacs-e.svg to a png: `rsvg-convert -h 256 emacs-e.svg > emacs-e.png`
(setq fancy-splash-image
      (concat doom-private-dir "splash/" "emacs-e.png"))

(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-footer)
(add-hook! '+doom-dashboard-mode-hook (hide-mode-line-mode 1) (hl-line-mode -1))
(setq-hook! '+doom-dashboard-mode-hook evil-normal-state-cursor (list nil))

(setq-default left-margin-width 2 right-margin-width 0)

(setq mac-mouse-wheel-smooth-scroll t)

(add-hook 'after-init-hook 'global-visual-line-mode)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

(setq evil-escape-key-sequence nil)

;; (defun my/evil-delete-advice (orig-fn beg end &optional type _ &rest args)
;;     "Make d, c, x to not write to clipboard."
;;     (apply orig-fn beg end type ?_ args))
;; (advice-add 'evil-delete :around 'my/evil-delete-advice)
;; (advice-add 'evil-change :around 'my/evil-delete-advice)

(bind-keys* ("C-h" . evil-window-left)
            ("C-j" . evil-window-down)
            ("C-k" . evil-window-up)
            ("C-l" . evil-window-right))

(bind-keys* ("C-c l" . evil-window-vsplit))
(bind-keys* ("C-c j" . evil-window-split))

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
      ;; Jupyter-related bindings
      (define-key map (kbd "j") #'my/jupyter)
      map)))

(add-hook 'python-mode-hook 'my/bind-python-keys)

(defun my/bind-python-lsp-keys ()
  (defalias 'my/lsp
    (let ((map (make-sparse-keymap)))
      ;; Restart lsp server
      (define-key map (kbd "r") #'lsp-workspace-restart)
      ;; Find definition
      (define-key map (kbd "f") #'lsp-find-definition)
      map)))

(add-hook 'python-mode-hook
          '(lambda ()
             (add-hook 'lsp-mode-hook
                       #'my/bind-python-lsp-keys)))

(defun my/bind-python-jupyter-keys ()
  (defalias 'my/jupyter
    (let ((map (make-sparse-keymap)))
      ;; Run jupyter REPL associated with current buffer
      (define-key map (kbd "R") #'jupyter-repl-associate-buffer)
      ;; Restart jupyter REPL
      (define-key map (kbd "r") #'jupyter-repl-restart-kernel)
      map)))

(add-hook 'python-mode-hook #'my/bind-python-jupyter-keys)

(defun my/bind-ess-r-keys ()
  (defalias 'my/<localleader>
    (let ((map (make-sparse-keymap)))
      ;; REPL
      (define-key map (kbd "R") #'run-ess-r)
      map)))

(add-hook 'ess-r-mode-hook 'my/bind-ess-r-keys)

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

(defun my/bind-markdown-keys ()
  (defalias 'my/<localleader>
    (let ((map (make-sparse-keymap)))
      ;; Format markdown table
      (define-key map (kbd "f") #'markdown-table-align)
      ;; Refresh toc
      (define-key map (kbd "r") #'markdown-toc-refresh-toc)
      map)))

(add-hook 'markdown-mode-hook 'my/bind-markdown-keys)

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

(add-hook 'jupyter-repl-mode-hook (lambda () (setq jupyter-repl-echo-eval-p t)))

(defun my/latex-format-environment-on-save ()
  (add-hook 'after-save-hook #'LaTeX-fill-environment))

(defun my/latexmk-on-save ()
  "Run LatexMk after saving .tex files"
  (add-hook 'after-save-hook 'my/latexmk))

(add-hook 'LaTeX-mode-hook 'my/latexmk-on-save)

(setq-default TeX-master nil)

(setq TeX-electric-sub-and-superscript nil)

(after! git-gutter
  (setq git-gutter:disabled-modes '(latex-mode)))

(after! tex
  (remove-hook 'TeX-update-style-hook #'rainbow-delimiters-mode))

;; Make default latex viewer pdf-tools
;; (setq +latex-viewers '(pdf-tools))

;; Use pdf-tools to open PDF files
;; (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
;;       TeX-source-correlate-start-server t)

;; Update PDF buffers after successful LaTeX runs
;; (add-hook 'TeX-after-compilation-finished-functions
;;           #'TeX-revert-document-buffer)

;; ;; Disable smartparens auto double-quoting in latex (https://emacs.stackexchange.com/questions/52233/disable-tex-modes-auto-tex-insert-quote-functionaliy)
;; ;; Uncommented because it was causing issues with org fancy priority. Might need to revisit.
;; (map! :after tex
;;       :map TeX-mode-map
;;       "\"" nil)
;; (after! smartparens-latex
;;   (sp-local-pair '(tex-mode plain-tex-mode latex-mode LaTeX-mode)
;;                   "``" "''" :actions :rem))

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
        '((sequence "IN-PROGRESS(p)" "TODO(t)" "WAITING(w)"
                    "IDEA(i)" "|" "DONE" "CANCELLED(c)"))))

;; Set other todo colors according to the nord theme (https://www.nordtheme.com/)
(setq org-todo-keyword-faces
      '(("IN-PROGRESS" . "#88C0D0")
        ("WAITING" . "#5E81AC")
        ("IDEA" . "#EBCB8B")
        ("CANCELED" . "#BF616A"))
      )

(setq org-log-done 'time)

(remove-hook 'org-mode-hook #'org-superstar-mode)

;; (after! org
;;   (setq org-fontify-quote-and-verse-blocks nil
;;         org-fontify-whole-heading-line nil
;;         org-hide-leading-stars nil
;;         org-startup-indented nil))

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

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-nord)
